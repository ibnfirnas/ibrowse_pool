-record(ibrowse_pool_spec,
    { name                   :: atom()
    , ssl_opts          = [] :: [term()]
    , max_sessions      = 10 :: pos_integer()
    , max_pipeline_size = 10 :: pos_integer()
    , max_attempts      = 3  :: pos_integer()
    }).
