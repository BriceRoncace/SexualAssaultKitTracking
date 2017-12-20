package gov.idaho.isp.saktrack.util;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import static java.nio.charset.StandardCharsets.UTF_8;
import javax.servlet.http.Cookie;

public class CookieUtils {

  public static Cookie createEncoded(String name, String value) {
    return new Cookie(name, encode(value));
  }

  public static String getDecodedValue(Cookie cookie) {
    return decode(cookie.getValue());
  }

  private static String encode(String string) {
    try {
      return URLEncoder.encode(string, UTF_8.toString());
    }
    catch (UnsupportedEncodingException ex) {
      throw new RuntimeException(ex);
    }
  }

  private static String decode(String string) {
    try {
      return URLDecoder.decode(string, UTF_8.toString());
    }
    catch (UnsupportedEncodingException ex) {
      throw new RuntimeException(ex);
    }
  }
}
