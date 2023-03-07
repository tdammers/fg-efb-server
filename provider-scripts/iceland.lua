local XML = require('xml')
local HTTP = require('http')
local URL = require('url')

local startURL = 'https://eaip.isavia.is/'

local function fetchHTML(url)
    return XML.parseHTML(HTTP.get(url))
end

local function intercalate(strings, sep)
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

function getPDF(path)
    return HTTP.download(URL.join(startURL, path), ".pdf")
end

function listFiles(path)
    local currentURL = startURL

    local function follow(query, attr)
        local doc = fetchHTML(currentURL)
        local nextURL = doc:query(query)[1].node:attr(attr)
        currentURL = URL.join(currentURL, nextURL)
    end

    if path == "" then
        follow('td.green a', 'href')
        follow('frame', 'src')
        follow('frame[name=eAISNavigation]', 'src')
        local doc = fetchHTML(currentURL)
        local qresult = doc:query("div#ADen-GBdetails .Hx>a[href][id~='2.24'][id$='en-GB']")
        local result = {}
        for k, v in ipairs(qresult) do
            local link = v.node
            local name = link:attr('href')
            name = string.gsub(name, "^BI.AD ", "")
            name = string.gsub(name, "([^-]*) - (.*) 1.*", function (cap1, ...) return cap1 end)
            local href = URL.join(currentURL, link:attr('href'))
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
        for k, v in ipairs(result) do
            print(v["path"])
        end
        return result
    else
        currentURL = URL.join(startURL, path)
        local doc = fetchHTML(currentURL)
        local qresult = doc:query("table.IndSubTable tr:has(a.ulink[href])")
        local result = {}
        for k, v in ipairs(qresult) do
            local linkRow = v.node
            local link = v:query("a.ulink[href]")[1].node
            local labelTD = v:query("td")[1].node
            local linkURL = URL.join(currentURL, link:attr('href'))
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
