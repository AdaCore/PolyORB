// gRPC Authentication Interceptor for Widget Core Service (C++)
// Addresses: Finding #3 (API Authentication/Authorization)
// Based on: ADR-002, RDB-001-Security-Addendum
// Location: src/security/auth_interceptor.cc

#include "auth_interceptor.h"
#include <grpcpp/grpcpp.h>
#include <jwt-cpp/jwt.h>
#include <openssl/pem.h>
#include <fstream>
#include <memory>
#include <string>
#include <vector>
#include <chrono>

namespace wxwidgets {
namespace security {

// JWT Validator class
class JWTValidator {
 public:
  explicit JWTValidator(const std::string& public_key_path) {
    LoadPublicKey(public_key_path);
  }

  struct Claims {
    std::string sub;  // Subject (user ID)
    std::string iss;  // Issuer
    std::vector<std::string> roles;
    std::vector<std::string> scopes;
    std::chrono::system_clock::time_point exp;  // Expiration
    bool is_valid{false};
  };

  Claims Validate(const std::string& token) {
    Claims claims;

    try {
      // Decode JWT
      auto decoded = jwt::decode(token);

      // Verify signature (RS256)
      auto verifier = jwt::verify()
          .allow_algorithm(jwt::algorithm::rs256(public_key_))
          .with_issuer("https://auth.refactorteam.local")
          .with_audience("wxwidgets-api");

      verifier.verify(decoded);

      // Extract claims
      claims.sub = decoded.get_subject();
      claims.iss = decoded.get_issuer();
      claims.exp = std::chrono::system_clock::from_time_t(
          decoded.get_expires_at().time_since_epoch().count());

      // Extract roles from custom claim
      if (decoded.has_payload_claim("roles")) {
        auto roles_claim = decoded.get_payload_claim("roles");
        if (roles_claim.get_type() == jwt::json::type::array) {
          for (const auto& role : roles_claim.as_array()) {
            claims.roles.push_back(role.get<std::string>());
          }
        }
      }

      // Extract scopes
      if (decoded.has_payload_claim("scope")) {
        auto scope_str = decoded.get_payload_claim("scope").as_string();
        // Parse space-separated scopes
        std::istringstream iss(scope_str);
        std::string scope;
        while (iss >> scope) {
          claims.scopes.push_back(scope);
        }
      }

      // Check expiration
      auto now = std::chrono::system_clock::now();
      if (claims.exp < now) {
        LOG(WARNING) << "JWT expired for user " << claims.sub;
        return claims;  // is_valid = false
      }

      claims.is_valid = true;
      return claims;

    } catch (const jwt::error::token_verification_exception& e) {
      LOG(ERROR) << "JWT verification failed: " << e.what();
      return claims;  // is_valid = false
    } catch (const std::exception& e) {
      LOG(ERROR) << "JWT processing error: " << e.what();
      return claims;  // is_valid = false
    }
  }

 private:
  void LoadPublicKey(const std::string& path) {
    std::ifstream key_file(path);
    if (!key_file.is_open()) {
      throw std::runtime_error("Failed to open JWT public key: " + path);
    }

    std::string key_content(
        (std::istreambuf_iterator<char>(key_file)),
        std::istreambuf_iterator<char>());

    public_key_ = key_content;
    LOG(INFO) << "Loaded JWT public key from " << path;
  }

  std::string public_key_;
};

// Authentication Interceptor
class AuthInterceptor : public grpc::experimental::Interceptor {
 public:
  explicit AuthInterceptor(
      std::shared_ptr<JWTValidator> jwt_validator,
      const std::vector<std::string>& required_roles)
      : jwt_validator_(jwt_validator),
        required_roles_(required_roles) {}

  void Intercept(grpc::experimental::InterceptorBatchMethods* methods) override {
    // Only intercept on receiving initial metadata (request headers)
    if (methods->QueryInterceptionHookPoint(
            grpc::experimental::InterceptionHookPoints::PRE_RECV_INITIAL_METADATA)) {

      auto metadata = methods->GetRecvInitialMetadata();

      // Extract Authorization header
      auto auth_header = metadata->find("authorization");
      if (auth_header == metadata->end()) {
        LOG(WARNING) << "Missing Authorization header";
        methods->FailWithError(grpc::Status(
            grpc::StatusCode::UNAUTHENTICATED,
            "Missing Authorization header. Provide: Authorization: Bearer <token>"));
        return;
      }

      // Extract Bearer token
      std::string token = ExtractBearerToken(auth_header->second);
      if (token.empty()) {
        LOG(WARNING) << "Invalid Authorization header format";
        methods->FailWithError(grpc::Status(
            grpc::StatusCode::UNAUTHENTICATED,
            "Invalid Authorization header. Expected: Bearer <token>"));
        return;
      }

      // Validate JWT
      auto claims = jwt_validator_->Validate(token);
      if (!claims.is_valid) {
        LOG(WARNING) << "Invalid JWT token";
        methods->FailWithError(grpc::Status(
            grpc::StatusCode::UNAUTHENTICATED,
            "Invalid or expired JWT token"));
        return;
      }

      // Check RBAC roles
      if (!HasRequiredRole(claims)) {
        LOG(WARNING) << "User " << claims.sub << " missing required roles. "
                     << "Has: [" << JoinStrings(claims.roles, ", ") << "], "
                     << "Needs: [" << JoinStrings(required_roles_, ", ") << "]";
        methods->FailWithError(grpc::Status(
            grpc::StatusCode::PERMISSION_DENIED,
            "Insufficient permissions. Required roles: " +
            JoinStrings(required_roles_, ", ")));
        return;
      }

      // Add user context to request metadata (for downstream processing)
      methods->AddTrailingMetadata("x-user-id", claims.sub);
      methods->AddTrailingMetadata("x-user-roles", JoinStrings(claims.roles, ","));
      methods->AddTrailingMetadata("x-user-scopes", JoinStrings(claims.scopes, ","));

      // Log successful authentication
      LOG(INFO) << "Authenticated user: " << claims.sub
                << " with roles: [" << JoinStrings(claims.roles, ", ") << "]";
    }

    // Proceed to next interceptor/handler
    methods->Proceed();
  }

 private:
  std::string ExtractBearerToken(const std::string& header_value) {
    const std::string prefix = "Bearer ";
    if (header_value.substr(0, prefix.size()) != prefix) {
      return "";
    }
    return header_value.substr(prefix.size());
  }

  bool HasRequiredRole(const JWTValidator::Claims& claims) {
    // If no specific roles required, allow any authenticated user
    if (required_roles_.empty()) {
      return true;
    }

    // Check if user has at least one required role
    for (const auto& required : required_roles_) {
      for (const auto& user_role : claims.roles) {
        if (user_role == required) {
          return true;
        }
      }
    }
    return false;
  }

  std::string JoinStrings(const std::vector<std::string>& vec,
                          const std::string& delimiter) {
    if (vec.empty()) return "";

    std::string result = vec[0];
    for (size_t i = 1; i < vec.size(); ++i) {
      result += delimiter + vec[i];
    }
    return result;
  }

  std::shared_ptr<JWTValidator> jwt_validator_;
  std::vector<std::string> required_roles_;
};

// Interceptor Factory
class AuthInterceptorFactory
    : public grpc::experimental::ServerInterceptorFactoryInterface {
 public:
  explicit AuthInterceptorFactory(
      std::shared_ptr<JWTValidator> jwt_validator,
      const std::vector<std::string>& required_roles)
      : jwt_validator_(jwt_validator),
        required_roles_(required_roles) {}

  grpc::experimental::Interceptor* CreateServerInterceptor(
      grpc::experimental::ServerRpcInfo* info) override {
    return new AuthInterceptor(jwt_validator_, required_roles_);
  }

 private:
  std::shared_ptr<JWTValidator> jwt_validator_;
  std::vector<std::string> required_roles_;
};

// Server builder helper
void AddAuthInterceptor(
    grpc::ServerBuilder& builder,
    const std::string& jwt_public_key_path,
    const std::vector<std::string>& required_roles) {

  auto jwt_validator = std::make_shared<JWTValidator>(jwt_public_key_path);

  std::vector<std::unique_ptr<grpc::experimental::ServerInterceptorFactoryInterface>>
      interceptor_creators;

  interceptor_creators.push_back(
      std::make_unique<AuthInterceptorFactory>(jwt_validator, required_roles));

  builder.experimental().SetInterceptorCreators(std::move(interceptor_creators));
}

}  // namespace security
}  // namespace wxwidgets

// Example Usage in main server:
/*
int main(int argc, char** argv) {
  std::string server_address("0.0.0.0:50051");
  std::string jwt_public_key_path("/mnt/secrets/jwt-public-key.pem");

  WidgetCoreServiceImpl service;

  grpc::ServerBuilder builder;
  builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
  builder.RegisterService(&service);

  // Add authentication interceptor (Finding #3)
  std::vector<std::string> required_roles = {
    "widget.creator",
    "widget.viewer"
  };
  wxwidgets::security::AddAuthInterceptor(
      builder, jwt_public_key_path, required_roles);

  std::unique_ptr<grpc::Server> server(builder.BuildAndStart());
  LOG(INFO) << "Server listening on " << server_address
            << " with JWT authentication";

  server->Wait();
  return 0;
}
*/

// CMakeLists.txt additions:
/*
find_package(jwt-cpp REQUIRED)
find_package(OpenSSL REQUIRED)

target_link_libraries(widget_core_service
  PRIVATE
    jwt-cpp::jwt-cpp
    OpenSSL::SSL
    OpenSSL::Crypto
    gRPC::grpc++
)
*/

// Dockerfile additions for jwt-cpp:
/*
RUN apt-get update && apt-get install -y \
    libjwt-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*
*/

// Testing (unit test example):
/*
TEST(AuthInterceptorTest, ValidJWT) {
  auto validator = std::make_shared<JWTValidator>("/path/to/test-public-key.pem");

  std::string test_jwt = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...";
  auto claims = validator->Validate(test_jwt);

  EXPECT_TRUE(claims.is_valid);
  EXPECT_EQ(claims.sub, "test_user_123");
  EXPECT_THAT(claims.roles, Contains("widget.creator"));
}

TEST(AuthInterceptorTest, ExpiredJWT) {
  auto validator = std::make_shared<JWTValidator>("/path/to/test-public-key.pem");

  std::string expired_jwt = "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9...";
  auto claims = validator->Validate(expired_jwt);

  EXPECT_FALSE(claims.is_valid);
}

TEST(AuthInterceptorTest, MissingAuthHeader) {
  // Mock gRPC context without Authorization header
  // Expect UNAUTHENTICATED status
}
*/
