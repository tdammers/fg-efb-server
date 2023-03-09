local XML = require 'xml'
local HTTP = require 'http'
local URL = require 'url'

require 'common'
require 'navigator'

local rootURL = "https://ops.skeyes.be"
local startURL = "/html/belgocontrol_static/eaip/eAIP_Main/html/eAIP/EB-menu-en-GB.html"

function getPDF(path)
    return HTTP.download(URL.join(rootURL, path), ".pdf")
end

function listFiles(path)
    local navigator = HTTPNavigator:new(rootURL)

    if path == "" then
        navigator:go(startURL)
        local qresult = navigator:query("#ADdetails .H3 a[href!=\"#\"][id^=\"AD-2\"]")
        local result = {}
        for k, v in ipairs(qresult) do
            local link = v.node
            local name = link.textContent
            name = string.gsub(name, utf8.char(160), ' ')
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
        navigator:go(path)
        local qresult = navigator:query("div[id$=\"2.24\"] tbody[id^=\"AD\"]>tr")
        local result = {}
        for k = 1, #qresult, 2 do
            local labelRow = qresult[k].node
            local linkRow = qresult[k+1].node
            local links = linkRow:query("a[href]:not([href^=\"mailto:\"])")
            if #links > 0 then
                local link = links[1].node
                local labelTD = labelRow:query("td")[1].node
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
        end
        return result
    end
end

