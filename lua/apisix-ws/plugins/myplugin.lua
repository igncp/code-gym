-- Introduce the module we need in the header
local log_util     =   require("apisix.utils.log-util")
local core         =   require("apisix.core")
local plugin       =   require("apisix.plugin")
local ngx          =   ngx

-- Declare the plugin's name
local plugin_name = "myplugin"

-- Define the plugin schema format
local schema = {
    type = "object",
    properties = {
        path = {
            type = "string"
        },
    },
    required = {"path"}
}

-- Plugin metadata schema
local metadata_schema = {
    type = "object",
    properties = {
        log_format = log_util.metadata_schema_log_format
    }
}

local _M = {
    version = 0.1,
    priority = 395,
    name = plugin_name,
    schema = schema,
    metadata_schema = metadata_schema
}

-- Check if the plugin configuration is correct
function _M.check_schema(conf, schema_type)
    if schema_type == core.schema.TYPE_METADATA then
        return core.schema.check(metadata_schema, conf)
    end
    return core.schema.check(schema, conf)
end

-- Log phase
function _M.log(conf, ctx)
    core.log.warn("Calling 'myplugin' log phase ________________________________________")
end

function _M.body_filter(conf, ctx)
    core.log.warn("Calling 'myplugin' body_filter phase _________________________________________")
end

return _M
