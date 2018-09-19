Testing
=======

This files serves as the entrypoint for the application's testing suite. To enable the spreading of tests across different files, it is necessary to enable _Hspec discovery_; this allows Hspec to view every module ending with "Spec" as a test module:

> {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

