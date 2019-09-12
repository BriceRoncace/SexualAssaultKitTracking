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

package gov.idaho.isp.saktrack.security;

import javax.servlet.http.HttpServletRequest;
import org.springframework.security.web.authentication.WebAuthenticationDetails;

public class CustomWebAuthenticationDetails extends WebAuthenticationDetails {
  private final String userAgent;

  public CustomWebAuthenticationDetails(HttpServletRequest req) {
    super(req);
    this.userAgent = req.getHeader("User-Agent");
  }

  public String getUserAgent() {
    return userAgent;
  }

  @Override
  public String toString() {
    return "CustomWebAuthenticationDetails{" + "userAgent=" + userAgent + ", " + super.toString() + '}';
  }
}
