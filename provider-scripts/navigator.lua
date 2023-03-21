local XML = require 'xml'
local HTTP = require 'http'
local URL = require 'url'

require 'common'

HTTPNavigator = {}

function HTTPNavigator:new (startURL)
    o = { currentURL = startURL; doc = nil }
    setmetatable(o, self)
    self.__index = self
    return o
end

function HTTPNavigator:fetch ()
    if self.doc == nil then
        self.doc, self.currentURL = fetchHTML(self.currentURL)
    end
    return self.doc, self.currentURL
end

function HTTPNavigator:follow (query, attr)
    local doc = self:fetch()
    local result = doc:query(query)
    if #result == 0 then
        print("Query not matched, not navigating anywhere")
        print(query)
        self.doc = nil
    else
        local nextURL = result[1].node:attr(attr)
        self:go(nextURL)
    end
    return self
end

function HTTPNavigator:go (url)
    self.currentURL = URL.join(self.currentURL, url)
    self.doc = nil
    return self
end

function HTTPNavigator:query (query)
    local doc = self:fetch()
    return doc:query(query)
end


