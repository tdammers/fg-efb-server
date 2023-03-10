local XML = require 'xml'
local HTTP = require 'http'
local URL = require 'url'

require 'common'
require 'navigator'

local rootURL = "https://eaip.lvnl.nl/"
local startURL = "https://www.lvnl.nl/informatie-voor-luchtvarenden/publicaties-voor-luchtvarenden"

function getPDF(path)
    return HTTP.download(URL.join(rootURL, path), ".pdf")
end

function listFiles(path)
    local navigator = HTTPNavigator:new(rootURL)

    if path == "" then
        navigator:go(startURL)
        navigator:follow("a[title=eAIP]", "href")
        navigator:follow("frame", "src")
        navigator:follow("frame[name=eAISMenuFrameset]", "src")
        navigator:follow("frame[name=eAISMenuContent]", "src")

        local qresult = navigator:query("div.level[id^='AD-2.'][id$='details']")
        local result = {}
        for k, v in ipairs(qresult) do
            local link = v:query("a[id$='2.24']")[1].node
            print(link)
            local name = link.textContent
            name = string.gsub(name, utf8.char(160), ' ')
            name = string.gsub(name, '^AD 2.24 ', '')
            name = string.gsub(name, '%s*CHARTS RELATED TO AN AERODROME$', '')
            local href = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(href).path, '/')

            table.insert(result,
                { type = "dir"
                ; name = name
                ; path = path
                }
            )
        end
        return result
    else
        navigator:go('/' .. path)
        local qresult = navigator:query("div[id$='2.24']>table>tbody>tr")
        local result = {}
        for k, v in ipairs(qresult) do
            print(v)
            local link = v:query("a[href]")[1].node
            local labelTD = v:query("td")[1].node
            local linkURL = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(linkURL).path, '/')
            local label = labelTD.textContent
            label = string.gsub(label, utf8.char(160), ' ')
            label = string.gsub(label, '^AD 2.', '')
            print(labelTD)
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


