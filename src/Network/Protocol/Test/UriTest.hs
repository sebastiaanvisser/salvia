module Main where

import Misc.Misc
import Network.Protocol.Uri
import Test.QuickCheck

-------[ path parsing tests ]--------------------------------------------------

propPathAbsoluteSingle      = (pPath @@ "/single")           == Just (Path True ["single"])
propPathAbsoluteMulti       = (pPath @@ "/first/second")     == Just (Path True ["first", "second"])
propPathAbsoluteMultiEnd    = (pPath @@ "/first/second/")    == Just (Path True ["first", "second", ""])
propPathAbsoluteMultiSkip   = (pPath @@ "/first//second//")  == Just (Path True ["first", "", "second", "", ""])

propPathDoubleSlash         = (pPath @@ "//wrong")           == Nothing
propPathEmpty               = (pPath @@ "")                  == Just (Path False [])

propPathRelativeSingle      = (pPath @@ "single")            == Just (Path False ["single"])
propPathRelativeMulti       = (pPath @@ "first/second")      == Just (Path False ["first", "second"])
propPathRelativeMultiEnd    = (pPath @@ "first/second/")     == Just (Path False ["first", "second", ""])
propPathRelativeMultiSkip   = (pPath @@ "first//second//")   == Just (Path False ["first", "", "second", "", ""])

propPathEscaped             = (pPath @@ "/space%20/at%40")   == Just (Path True ["space ", "at@"])
propPathSubDelims           = (pPath @@ "/subs!$&'()*+,;=/") == Just (Path True ["subs!$&'()*+,;=", ""])
propPathAtColon             = (pPath @@ "/a@t/co:lon/")      == Just (Path True ["a@t", "co:lon", ""])
propPathUnreserved          = (pPath @@ "/ok-._~ok/")        == Just (Path True ["ok-._~ok", ""])

-------[ host parsing tests ]--------------------------------------------------

propHostIpv4Local          = (pHost @@ "127.0.0.1")         == Just (IPv4 [127, 0, 0, 1])
propHostIpv4Min            = (pHost @@ "0.0.0.0")           == Just (IPv4 [0, 0, 0, 0])
propHostIpv4Max            = (pHost @@ "255.255.255.255")   == Just (IPv4 [255, 255, 255, 255])

propHostIpv4TooSmall       = (pHost @@ "127.0.0")           == Just (RegName "127.0.0")
propHostIpv4IsRegname      = (pHost @@ "127.0.0.1.2")       == Just (RegName "127.0.0.1.2")
propHostIpv4IsHostname     = (pHost @@ "127.0.0.1.org")     == Just (Hostname ["127", "0", "0", "1", "org"])

propHostHostnameRegular    = (pHost @@ "sub-0.domain.com")  == Just (Hostname ["sub-0", "domain", "com"])
propHostHostnameSingleton  = (pHost @@ "domain")            == Just (Hostname ["domain"])
propHostHostnameNumerals   = (pHost @@ "0.1.2.com")         == Just (Hostname ["0", "1", "2", "com"])
propHostRegnameNumerals    = (pHost @@ "012.123.2")         == Just (RegName "012.123.2")
propHostRegnameUnreserved  = (pHost @@ "__reg~name.reg")    == Just (RegName "__reg~name.reg")
propHostRegnameError       = (pHost @@ "err@r%$")           == Nothing

-------[ total uri parsing ]---------------------------------------------------

uriTotal      = "http://user:pass@sub.domain.top:1337/wiki/first%20section/ch%61pter-2;pdf?additional=vars#sharp"
uriDomainOnly = "ssh://sub.domain.top"
uriPathOnly   = "/absolute/path"
uriExample0   = "ftp://ftp.is.co.za/rfc/rfc1808.txt"
uriExample1   = "http://www.ietf.org/rfc/rfc2396.txt"
uriExample2   = "ldap://[2001:db8::7]/c=GB?objectClass?one"
uriExample3   = "mailto:John.Doe@example.com"
uriExample4   = "news:comp.infosystems.www.servers.unix"
uriExample5   = "tel:+1-816-555-1212"
uriExample6   = "telnet://192.0.2.16:80/"
uriExample7   = "urn:oasis:names:specification:docbook:dtd:xml:4.1.2"

propUriTotal = parseURI uriTotal ==
  URI False "http"
    (Authority "user:pass" (Hostname ["sub","domain","top"]) 1337)
    (Path True  ["wiki","first section","chapter-2;pdf"])
    "additional=vars" "sharp"

propUriDomainOnly = parseURI uriDomainOnly ==
  URI False "ssh"
    (Authority "" (Hostname ["sub","domain","top"]) (-1))
    (Path True [])
    "" ""

propUriPathOnly = parseURI uriPathOnly ==
  URI True ""
    (Authority "" (Hostname []) (-1))
    (Path True ["absolute", "path"])
    "" ""

-- Example from rfc3986 - 1.1.2. Examples
propExample0 = parseURI uriExample0 ==
  URI False "ftp" 
    (Authority "" (Hostname ["ftp","is","co","za"]) (-1))
    (Path True ["rfc","rfc1808.txt"])
    "" ""

propExample1 = parseURI uriExample1 ==
  URI False "http"
    (Authority "" (Hostname ["www","ietf","org"]) (-1))
    (Path True ["rfc","rfc2396.txt"])
    "" ""

propExample2 = parseURI uriExample2 ==
  URI True ""
    (Authority "" (Hostname []) (-1))
    (Path False ["ldap:","",""])
    "" ""

propExample3 = parseURI uriExample3 ==
  URI False "mailto"
    (Authority "" (Hostname []) (-1))
    (Path False ["John.Doe@example.com"])
    "" ""

propExample4 = parseURI uriExample4 ==
  URI False "news"
    (Authority "" (Hostname []) (-1))
    (Path False ["comp.infosystems.www.servers.unix"])
    "" ""

propExample5 = parseURI uriExample5 ==
  URI False "tel"
    (Authority "" (Hostname []) (-1))
    (Path False ["+1-816-555-1212"])
    "" ""

propExample6 = parseURI uriExample6 ==
  URI False "telnet"
    (Authority "" (IPv4 [192,0,2,16]) 80)
    (Path True [""])
    "" ""

propExample7 = parseURI uriExample7 ==
  URI False "urn"
    (Authority "" (Hostname []) (-1))
    (Path False ["oasis:names:specification:docbook:dtd:xml:4.1.2"])
    "" ""

-------[ run all tests ]-------------------------------------------------------

main = testAll $ map (\(a, b) -> (a, unitCheck b)) [

    ("parse/propPathAbsoluteSingle",    propPathAbsoluteSingle)
  , ("parse/propPathAbsoluteMulti",     propPathAbsoluteMulti)
  , ("parse/propPathAbsoluteMultiEnd",  propPathAbsoluteMultiEnd)
  , ("parse/propPathAbsoluteMultiSkip", propPathAbsoluteMultiSkip)
  , ("parse/propPathDoubleSlash",       propPathDoubleSlash)
  , ("parse/propPathEmpty",             propPathEmpty)
  , ("parse/propPathRelativeSingle",    propPathRelativeSingle)
  , ("parse/propPathRelativeMulti",     propPathRelativeMulti)
  , ("parse/propPathRelativeMultiEnd",  propPathRelativeMultiEnd)
  , ("parse/propPathRelativeMultiSkip", propPathRelativeMultiSkip)
  , ("parse/propPathEscaped",           propPathEscaped)
  , ("parse/propPathSubDelims",         propPathSubDelims)
  , ("parse/propPathAtColon",           propPathAtColon)
  , ("parse/propPathUnreserved",        propPathUnreserved)

  , ("parse/propHostIpv4Local",         propHostIpv4Local)
  , ("parse/propHostIpv4Min",           propHostIpv4Min)
  , ("parse/propHostIpv4Max",           propHostIpv4Max)
  , ("parse/propHostIpv4TooSmall",      propHostIpv4TooSmall)
  , ("parse/propHostIpv4IsRegname",     propHostIpv4IsRegname)
  , ("parse/propHostIpv4IsHostname",    propHostIpv4IsHostname)
  , ("parse/propHostHostnameRegular",   propHostHostnameRegular)
  , ("parse/propHostHostnameSingleton", propHostHostnameSingleton)
  , ("parse/propHostHostnameNumerals",  propHostHostnameNumerals)
  , ("parse/propHostRegnameNumerals",   propHostRegnameNumerals)
  , ("parse/propHostRegnameUnreserved", propHostRegnameUnreserved)
  , ("parse/propHostRegnameError",      propHostRegnameError)

  , ("parse/propUriTotal",              propUriTotal)
  , ("parse/propUriDomainOnly",         propUriDomainOnly)
  , ("parse/propUriPathOnly",           propUriPathOnly)
  , ("parse/propExample1",              propExample1)
  , ("parse/propExample2",              propExample2)
  , ("parse/propExample3",              propExample3)
  , ("parse/propExample4",              propExample4)
--   , ("parse/propExample5",           propExample5)
  , ("parse/propExample6",              propExample6)
  , ("parse/propExample7",              propExample7)
  ]

