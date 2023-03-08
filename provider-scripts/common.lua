local XML = require 'xml'
local HTTP = require 'http'
local URL = require 'url'

function fetchHTML(url)
    body, finalURL = HTTP.get(url)
    return XML.parseHTML(body), finalURL
end

function intercalate(strings, sep)
    local result = ""

    for k, v in ipairs(strings) do
        if result == "" then
            result = v
        else
            result = result .. sep .. v
        end
    end

    return result
end
