.. _Performance:

**************************
Performance Considerations
**************************

This section discusses performance when using PolyORB. Many elements
can be configured, :ref:`Building_an_application_with_PolyORB`.
By carefully selecting them, you can increase the
throughput of your application.

We review some parameters that can impact performance.

* **Build options**:

  * For production use, you should not build PolyORB with debug
    activated.

* **Tasking policies**:

  * You should carefully select the tasking policy to reduce
    dynamic ressource allocation (tasks, entry points,
    etc.). :ref:`Tasking_model_in_PolyORB`.

* **Transport parameters**:

  * Setting `tcp.nodelay` to false will disable Nagle buffering.

* **GIOP parameters**:

  * Setting
    `polyorb.protocols.iiop.giop.1.X.locate_then_request`, where
    `X` is the GIOP version in use, to false will disable
    `Locate_Message`, reducing the number of requests exchanged,

  * Increasing
    `polyorb.protocols.iiop.giop.1.X.max_message_size`, where
    `X` is the GIOP version in use, will reduce GIOP fragmentation,
    reducing middleware processing.

