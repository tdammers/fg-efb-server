local XML = require 'xml'
local HTTP = require 'http'
local URL = require 'url'

require 'common'
require 'navigator'

local rootURL = 'https://skyvector.com'
local startURL = 'https://skyvector.com/airports/United+States'

function getPDF(path)
    return HTTP.download(URL.join(rootURL, path), ".pdf")
end

function listFiles(path)
    local navigator = HTTPNavigator:new(startURL)

    if path == "" then
        local qresult = navigator:query('.view-content>.views-summary>a:not([href$="/United%20States/"])')
        local result = {}
        for k, v in ipairs(qresult) do
            local link = v.node
            local href = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(href).path, '/')

            local name = link.textContent

            table.insert(result,
                { type = "dir"
                ; name = name
                ; path = path
                }
            )
        end
        return result
    elseif string.match(path, '^airports/') ~= fail then
        navigator:go('/' .. path)
        local qresult = navigator:query('td.views-field-title>a')
        dumpTable(qresult)
        local result = {}
        for k, v in ipairs(qresult) do
            local link = v.node
            local href = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(href).path, '/')

            local name = link.textContent

            table.insert(result,
                { type = "dir"
                ; name = name
                ; path = path
                }
            )
        end

        function isICAOEquiv (name)
            local code = string.match(name, '^[%u%d]*')
            local hasNumbers = string.match(code, '%d') ~= fail
            if hasNumbers or #code ~= 3 then
                return 0
            else
                return 1
            end
        end

        table.sort(result, function (a, b)
            local aIEA = isICAOEquiv(a.name)
            local bIEA = isICAOEquiv(b.name)

            if aIEA == bIEA then
                return a.name < b.name
            else
                return aIEA > bIEA -- list ICAO-equivs first
            end
        end)
        return result
    elseif string.match(path, '^airport/') ~= fail then
        navigator:go('/' .. path)
        local result = {}

        local qresult = navigator:query('div.aptdata>a:has(img)')
        for k, v in ipairs(qresult) do
            local name = v:query('img')[1].node:attr('alt')
            local href = URL.join(navigator.currentURL, v.node:attr('href'))
            local path = intercalate(URL.parse(href).path, '/')
            table.insert(result,
                { type = "pdf"
                ; name = name
                ; path = path
                }
            )
        end

        local qresult = navigator:query('.aptdata>ul>li.apttpp>a,.aptdata #aptafd>a')
        for k, v in ipairs(qresult) do
            local name = v.node.textContent
            if name == '' then
                name = v:query('img')[1].node:attr('alt')
            end
            local href = URL.join(navigator.currentURL, v.node:attr('href'))
            local path = intercalate(URL.parse(href).path, '/')
            table.insert(result,
                { type = "pdf"
                ; name = name
                ; path = path
                }
            )

        end

        return result
    end
end
