-record(ibrowse_pool_spec,
    { name              :: atom()
    , ssl_opts          :: [term()]
    , max_sessions      :: pos_integer()
    , max_pipeline_size :: pos_integer()
    , max_attempts      :: pos_integer()
    , timeout           :: timeout()
    }).
