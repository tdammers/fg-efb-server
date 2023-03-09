local XML = require 'xml'
local HTTP = require 'http'
local URL = require 'url'

require 'common'
require 'navigator'

local rootURL = 'https://eaip.isavia.is/'

function getPDF(path)
    return HTTP.download(URL.join(rootURL, path), ".pdf")
end

function listFiles(path)
    local navigator = HTTPNavigator:new(rootURL)

    if path == "" then
        navigator:follow('td.green a', 'href')
        navigator:follow('frame', 'src')
        navigator:follow('frame[name=eAISNavigation]', 'src')
        local qresult = navigator:query("div#ADen-GBdetails .Hx>a[href][id~='2.24'][id$='en-GB']")
        local result = {}
        for k, v in ipairs(qresult) do
            local link = v.node
            local name = link:attr('href')
            name = string.gsub(name, "^BI.AD ", "")
            name = string.gsub(name, "([^-]*) - (.*) 1.*", function (cap1, ...) return cap1 end)
            local href = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(href).path, '/')

            if string.match(link.textContent, 'CHARTS RELATED TO AERODROME') then
                table.insert(result,
                    { type = "dir"
                    ; name = name
                    ; path = path
                    }
                )
            end
        end
        return result
    else
        navigator:go(path)
        local qresult = navigator:query("table.IndSubTable tr:has(a.ulink[href])")
        local result = {}
        for k, v in ipairs(qresult) do
            local linkRow = v.node
            local link = v:query("a.ulink[href]")[1].node
            local labelTD = v:query("td")[1].node
            local linkURL = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(linkURL).path, '/')
            local label = string.gsub(
                            labelTD.textContent,
                            '%s+', ' ')
            table.insert(result,
                { type = "pdf"
                ; name = label
                ; path = path
                }
            )
        end
        return result
    end
end
