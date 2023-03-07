xml = require("xml")
http = require("http")

function listFiles(path)
    body = http.get("https://tobiasdammers.nl/")
    doc = xml.parseHTML(body)
    print(doc:query("div")[1].node)
    print(doc:query("div")[1].node:attr("class"))
    return {
        { type = "dir"
        , name = "test directory"
        , path = "test"
        }
    }
end
