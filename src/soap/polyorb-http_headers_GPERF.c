/* C code produced by gperf version 2.7.2 */
/* Command-line: gperf -N polyorb__http_headers__IN_WORD_SET -t polyorb-http_headers.perf  */
enum kwids {
  H_Cache_Control, /* "Cache-Control" */
  H_Connection, /* "Connection" */
  H_Date, /* "Date" */
  H_Pragma, /* "Pragma" */
  H_Trailer, /* "Trailer" */
  H_Transfer_Encoding, /* "Transfer-Encoding" */
  H_Upgrade, /* "Upgrade" */
  H_Via, /* "Via" */
  H_Warning, /* "Warning" */
  H_Accept, /* "Accept" */
  H_Accept_Charset, /* "Accept-Charset" */
  H_Accept_Language, /* "Accept-Language" */
  H_Authorization, /* "Authorization" */
  H_Expect, /* "Expect" */
  H_From, /* "From" */
  H_Host, /* "Host" */
  H_If_Match, /* "If-Match" */
  H_If_Modified_Since, /* "If-Modified-Since" */
  H_If_None_Match, /* "If-None-Match" */
  H_If_Range, /* "If-Range" */
  H_If_Unmodified_Since, /* "If-Unmodified-Since" */
  H_Max_Forwards, /* "Max-Forwards" */
  H_Proxy_Authorization, /* "Proxy-Authorization" */
  H_Range, /* "Range" */
  H_Referer, /* "Referer" */
  H_TE, /* "TE" */
  H_User_Agent, /* "User-Agent" */
  H_Accept_Ranges, /* "Accept-Ranges" */
  H_Age, /* "Age" */
  H_ETag, /* "ETag" */
  H_Location, /* "Location" */
  H_Proxy_Authenticate, /* "Proxy-Authenticate" */
  H_Retry_After, /* "Retry-After" */
  H_Server, /* "Server" */
  H_Vary, /* "Vary" */
  H_WWW_Authenticate, /* "WWW-Authenticate" */
  H_Allow, /* "Allow" */
  H_Content_Encoding, /* "Content-Encoding" */
  H_Content_Language, /* "Content-Language" */
  H_Content_Length, /* "Content-Length" */
  H_Content_Location, /* "Content-Location" */
  H_Content_MD5, /* "Content-MD5" */
  H_Content_Range, /* "Content-Range" */
  H_Content_Type, /* "Content-Type" */
  H_Expires, /* "Expires" */
  H_Last_Modified, /* "Last-Modified" */
  H_SOAPAction, /* "SOAPAction" */
  Extension_Header, /* no-match case */
};
struct kwinfo { char *name; enum kwids id; };

#define TOTAL_KEYWORDS 47
#define MIN_WORD_LENGTH 2
#define MAX_WORD_LENGTH 19
#define MIN_HASH_VALUE 8
#define MAX_HASH_VALUE 125
/* maximum key range = 118, duplicates = 0 */

#ifdef __GNUC__
__inline
#else
#ifdef __cplusplus
inline
#endif
#endif
static unsigned int
hash (str, len)
     register const char *str;
     register unsigned int len;
{
  static unsigned char asso_values[] =
    {
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126,   0, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126,  50, 126,   0,
       20,   0,   5,   5,  25,   0, 126, 126,  45,  25,
       50, 126,  25, 126,  25,  25,  61,  15,  15,  60,
      126,  30, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126, 126, 126, 126, 126,
      126, 126, 126, 126, 126, 126
    };
  return len + asso_values[(unsigned char)str[len - 1]] + asso_values[(unsigned char)str[0]];
}

#ifdef __GNUC__
__inline
#endif
struct kwinfo *
polyorb__http_headers__IN_WORD_SET (str, len)
     register const char *str;
     register unsigned int len;
{
  static struct kwinfo wordlist[] =
    {
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"if-range", H_If_Range},
      {"etag", H_ETag},
      {""},
      {"content-md5", H_Content_MD5},
      {"content-type", H_Content_Type},
      {"content-range", H_Content_Range},
      {""}, {""},
      {"content-language", H_Content_Language},
      {"if-modified-since", H_If_Modified_Since},
      {""},
      {"if-unmodified-since", H_If_Unmodified_Since},
      {""},
      {"content-encoding", H_Content_Encoding},
      {"upgrade", H_Upgrade},
      {""},
      {"date", H_Date},
      {""}, {""}, {""}, {""}, {""},
      {"range", H_Range},
      {""},
      {"expires", H_Expires},
      {"if-match", H_If_Match},
      {"from", H_From},
      {""}, {""}, {""},
      {"if-none-match", H_If_None_Match},
      {"content-length", H_Content_Length},
      {""}, {""}, {""},
      {"proxy-authenticate", H_Proxy_Authenticate},
      {""}, {""}, {""}, {""}, {""},
      {"vary", H_Vary},
      {""}, {""}, {""},
      {"age", H_Age},
      {""}, {""},
      {"server", H_Server},
      {"referer", H_Referer},
      {"cache-control", H_Cache_Control},
      {""},
      {"connection", H_Connection},
      {"retry-after", H_Retry_After},
      {"max-forwards", H_Max_Forwards},
      {"te", H_TE},
      {""},
      {"accept-language", H_Accept_Language},
      {"content-location", H_Content_Location},
      {"expect", H_Expect},
      {"via", H_Via},
      {""}, {""}, {""},
      {"warning", H_Warning},
      {""}, {""}, {""},
      {"www-authenticate", H_WWW_Authenticate},
      {""},
      {"last-modified", H_Last_Modified},
      {""}, {""},
      {"pragma", H_Pragma},
      {""},
      {"transfer-encoding", H_Transfer_Encoding},
      {""},
      {"soapaction", H_SOAPAction},
      {"user-agent", H_User_Agent},
      {""},
      {"accept-ranges", H_Accept_Ranges},
      {""},
      {"host", H_Host},
      {""}, {""},
      {"trailer", H_Trailer},
      {"proxy-authorization", H_Proxy_Authorization},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"location", H_Location},
      {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"authorization", H_Authorization},
      {""},
      {"allow", H_Allow},
      {""},
      {"accept", H_Accept},
      {""}, {""}, {""}, {""}, {""}, {""}, {""},
      {"accept-charset", H_Accept_Charset}
    };

  if (len <= MAX_WORD_LENGTH && len >= MIN_WORD_LENGTH)
    {
      register int key = hash (str, len);

      if (key <= MAX_HASH_VALUE && key >= 0)
        {
          register const char *s = wordlist[key].name;

          if (*str == *s && !strcmp (str + 1, s + 1))
            return &wordlist[key];
        }
    }
  return 0;
}
