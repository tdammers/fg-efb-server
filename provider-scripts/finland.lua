local XML = require 'xml'
local HTTP = require 'http'
local URL = require 'url'

require 'common'
require 'navigator'

local rootURL = 'https://www.ais.fi/'

function getPDF(path)
    return HTTP.download(URL.join(rootURL, path), ".pdf")
end

function listFiles(path)
    local navigator = HTTPNavigator:new(rootURL)

    if path == "" then
        navigator:go('/eaip/')
        local link = navigator:query('table.HISTORY a')[1].node
        print(link)
        local nextURL = link:attr('href')
        nextURL = string.gsub(nextURL, 'index.html$', '/index.html')
        print(nextURL)
        navigator:go(nextURL)
        navigator:follow('frame[name=eAISNavigationBase]', 'src')
        navigator:follow('frame[name=eAISNavigation]', 'src')
        -- We query the document in 2 steps, because it is pretty massive (170k lines, ~10 MiB).
        local div = navigator:query("div[id='AD 2en-GBdetails']")[1].node
        local qresult = div:query(".Hx a[href][title$='LENTOASEMAA KOSKEVAT KARTAT']")
        local result = {}
        for k, v in ipairs(qresult) do
            local link = v.node
            local name = link.textContent
            local label = link:attr('href')
            local href = URL.join(navigator.currentURL, link:attr('href'))
            local path = intercalate(URL.parse(href).path, '/')

            if (string.match(name, 'CHARTS RELATED TO THE AERODROME') ~= nil) then
                table.insert(result,
                    { type = "dir"
                    ; name = string.gsub(label, '^EF[\\-]AD 2 (.*)1[\\-]en[\\-]GB.html#.*$', '%1')
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
