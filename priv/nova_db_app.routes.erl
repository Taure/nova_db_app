#{prefix => "",
  security => false,
  routes => [
            {"/message", { nova_db_app_message_controller, get_all}, #{methods => [get]}},
            {"/message", { nova_db_app_message_controller, create_message}, #{methods => [post]}},
            {"/message/:messageid", { nova_db_app_message_controller, get_message}, #{methods => [get]}},
            {"/message/:messageid", { nova_db_app_message_controller, update_message}, #{methods => [put]}},
            {"/message/:messageid", { nova_db_app_message_controller, delete_message}, #{methods => [delete]}}
           ]
}.
