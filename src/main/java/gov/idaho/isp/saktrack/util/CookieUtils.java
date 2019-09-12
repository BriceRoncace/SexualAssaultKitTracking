/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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
