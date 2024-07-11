use std::fmt::Debug;

use serde::Deserialize;

use webar_data::ser::Serialize;

macro_rules! headers  {
    (enum $n:ident { $($hn:ident = $v:literal,)+ }) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        enum $n {
            $($hn,)+
        }
        impl $n {
            fn from_str(v: &str) -> Option<Self> {
                static MAP:phf::Map<&'static str, $n> = phf::phf_map!{
                    $($v => $n::$hn,)+
                };
                MAP.get(v).copied()
            }
            const fn as_str(&self) ->&'static str {
                match self {
                    $(Self::$hn => $v,)+
                }
            }
        }
    };
}
headers!(
    enum Standard {
        Accept = "accept",
        AcceptCH = "accept-ch",
        AcceptCharset = "accept-charset",
        AcceptEncoding = "accept-encoding",
        AcceptLanguage = "accept-language",
        AcceptPatch = "accept-patch",
        AcceptPost = "accept-post",
        AcceptRanges = "accept-ranges",
        AccessControlAllowCredentials = "access-control-allow-credentials",
        AccessControlAllowHeaders = "access-control-allow-headers",
        AccessControlAllowMethods = "access-control-allow-methods",
        AccessControlAllowOrigin = "access-control-allow-origin",
        AccessControlExposeHeaders = "access-control-expose-headers",
        AccessControlMaxAge = "access-control-max-age",
        AccessControlRequestHeaders = "access-control-request-headers",
        AccessControlRequestMethod = "access-control-request-method",
        Age = "age",
        Allow = "allow",
        AltSvc = "alt-svc",
        AltUsed = "alt-used",
        AttributionReportingEligible = "attribution-reporting-eligible",
        AttributionReportingRegisterSource = "attribution-reporting-register-source",
        AttributionReportingRegisterTrigger = "attribution-reporting-register-trigger",
        Authorization = "authorization",
        CacheControl = "cache-control",
        ClearSiteData = "clear-site-data",
        Connection = "connection",
        ContentDigest = "content-digest",
        ContentDisposition = "content-disposition",
        ContentDPR = "content-dpr",
        ContentEncoding = "content-encoding",
        ContentLanguage = "content-language",
        ContentLength = "content-length",
        ContentLocation = "content-location",
        ContentRange = "content-range",
        ContentSecurityPolicy = "content-security-policy",
        ContentSecurityPolicyReportOnly = "content-security-policy-report-only",
        ContentType = "content-type",
        Cookie = "cookie",
        CriticalCH = "critical-ch",
        CrossOriginEmbedderPolicy = "cross-origin-embedder-policy",
        CrossOriginOpenerPolicy = "cross-origin-opener-policy",
        CrossOriginResourcePolicy = "cross-origin-resource-policy",
        Date = "date",
        DeviceMemory = "device-memory",
        Digest = "digest",
        DNT = "dnt",
        Downlink = "downlink",
        DPR = "dpr",
        EarlyData = "early-data",
        ECT = "ect",
        ETag = "etag",
        Expect = "expect",
        ExpectCT = "expect-ct",
        Expires = "expires",
        Forwarded = "forwarded",
        From = "from",
        Host = "host",
        IfMatch = "if-match",
        IfModifiedSince = "if-modified-since",
        IfNoneMatch = "if-none-match",
        IfRange = "if-range",
        IfUnmodifiedSince = "if-unmodified-since",
        KeepAlive = "keep-alive",
        LargeAllocation = "large-allocation",
        LastModified = "last-modified",
        Link = "link",
        Location = "location",
        MaxForwards = "max-forwards",
        NEL = "nel",
        NoVarySearch = "no-vary-search",
        ObserveBrowsingTopics = "observe-browsing-topics",
        Origin = "origin",
        OriginAgentCluster = "origin-agent-cluster",
        PermissionsPolicy = "permissions-policy",
        Pragma = "pragma",
        ProxyAuthenticate = "proxy-authenticate",
        ProxyAuthorization = "proxy-authorization",
        Range = "range",
        Referer = "referer",
        ReferrerPolicy = "referrer-policy",
        ReportingEndpoints = "reporting-endpoints",
        ReprDigest = "repr-digest",
        RetryAfter = "retry-after",
        RTT = "rtt",
        SaveData = "save-data",
        SecBrowsingTopics = "sec-browsing-topics",
        SecCHPrefersColorScheme = "sec-ch-prefers-color-scheme",
        SecCHPrefersReducedMotion = "sec-ch-prefers-reduced-motion",
        SecCHPrefersReducedTransparency = "sec-ch-prefers-reduced-transparency",
        SecCHUA = "sec-ch-ua",
        SecCHUAArch = "sec-ch-ua-arch",
        SecCHUABitness = "sec-ch-ua-bitness",
        SecCHUAFullVersion = "sec-ch-ua-full-version",
        SecCHUAFullVersionList = "sec-ch-ua-full-version-list",
        SecCHUAMobile = "sec-ch-ua-mobile",
        SecCHUAModel = "sec-ch-ua-model",
        SecCHUAPlatform = "sec-ch-ua-platform",
        SecCHUAPlatformVersion = "sec-ch-ua-platform-version",
        SecFetchDest = "sec-fetch-dest",
        SecFetchMode = "sec-fetch-mode",
        SecFetchSite = "sec-fetch-site",
        SecFetchUser = "sec-fetch-user",
        SecGPC = "sec-gpc",
        SecPurpose = "sec-purpose",
        SecWebSocketAccept = "sec-websocket-accept",
        Server = "server",
        ServerTiming = "server-timing",
        ServiceWorkerNavigationPreload = "service-worker-navigation-preload",
        SetCookie = "set-cookie",
        SetLogin = "set-login",
        SourceMap = "sourcemap",
        SpeculationRules = "speculation-rules",
        StrictTransportSecurity = "strict-transport-security",
        SupportsLoadingMode = "supports-loading-mode",
        TE = "te",
        TimingAllowOrigin = "timing-allow-origin",
        Tk = "tk",
        Trailer = "trailer",
        TransferEncoding = "transfer-encoding",
        Upgrade = "upgrade",
        UpgradeInsecureRequests = "upgrade-insecure-requests",
        UserAgent = "user-agent",
        Vary = "vary",
        Via = "via",
        ViewportWidth = "viewport-width",
        WantContentDigest = "want-content-digest",
        WantDigest = "want-digest",
        WantReprDigest = "want-repr-digest",
        Warning = "warning",
        Width = "width",
        WWWAuthenticate = "www-authenticate",
        XContentTypeOptions = "x-content-type-options",
        XDNSPrefetchControl = "x-dns-prefetch-control",
        XForwardedFor = "x-forwarded-for",
        XForwardedHost = "x-forwarded-host",
        XForwardedProto = "x-forwarded-proto",
        XFrameOptions = "x-frame-options",
        XXSSProtection = "x-xss-protection",
    }
);

#[derive(Clone, PartialEq, Eq, Hash)]
enum Inner {
    Standard(Standard),
    Custom(String),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct HeaderName(Inner);
impl HeaderName {
    pub fn as_str(&self) -> &str {
        match &self.0 {
            Inner::Standard(s) => s.as_str(),
            Inner::Custom(c) => c.as_str(),
        }
    }
    pub(super) fn from_str(v: &str) -> Self {
        match Standard::from_str(v) {
            Some(s) => Self(Inner::Standard(s)),
            None => Self(Inner::Custom(v.to_owned())),
        }
    }
}
impl Debug for HeaderName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}
impl PartialOrd for HeaderName {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (&self.0, &other.0) {
            (Inner::Standard(s1), Inner::Standard(s2)) => s1.partial_cmp(s2),
            _ => self.as_str().partial_cmp(other.as_str()),
        }
    }
}
impl Ord for HeaderName {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (&self.0, &other.0) {
            (Inner::Standard(s1), Inner::Standard(s2)) => s1.cmp(s2),
            _ => self.as_str().cmp(other.as_str()),
        }
    }
}
impl From<http::HeaderName> for HeaderName {
    fn from(value: http::HeaderName) -> Self {
        Self::from_str(value.as_str())
    }
}

impl Serialize for HeaderName {
    fn serialize<S: webar_data::ser::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(self.as_str())
    }
}
impl<'de> Deserialize<'de> for HeaderName {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = HeaderName;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("http header name")
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(HeaderName::from_str(v))
            }
            fn visit_string<E>(self, v: String) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match Standard::from_str(&v) {
                    Some(s) => Ok(HeaderName(Inner::Standard(s))),
                    None => Ok(HeaderName(Inner::Custom(v))),
                }
            }
        }
        deserializer.deserialize_string(Visitor)
    }
}

#[cfg(test)]
mod tests {
    use std::cmp::Ordering;

    use super::{HeaderName, Inner, Standard};

    #[test]
    fn standard_ord() {
        assert_eq!(
            HeaderName(Inner::Standard(Standard::Server))
                .cmp(&HeaderName(Inner::Standard(Standard::Accept))),
            Ordering::Greater
        );
    }

    #[test]
    fn custom_standard() {
        assert_eq!(
            HeaderName(Inner::Standard(Standard::Cookie))
                .cmp(&HeaderName(Inner::Custom("a".into()))),
            Ordering::Greater
        );
        assert_eq!(
            HeaderName(Inner::Standard(Standard::Vary))
                .cmp(&HeaderName(Inner::Custom("x-hdr".into()))),
            Ordering::Less
        );
    }
}
