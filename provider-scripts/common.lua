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

function dumpTable(table, indent)
    indent = indent or ''
    print(indent .. "{")
    for k, v in pairs(table) do
        if type(v) == 'table' then
            print(string.format('%s  %s = ', indent, k))
            dumpTable(v, indent .. "    ")
        else
            print(string.format('%s  %s = %q', indent, k, v))
        end
    end
    print(indent .. "}")
end
