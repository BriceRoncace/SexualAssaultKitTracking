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

package gov.idaho.isp.saktrack.domain.user.password.dto;

import gov.idaho.isp.saktrack.validation.PasswordPolicy;
import gov.idaho.isp.saktrack.validation.PropertiesEqual;
import jakarta.validation.constraints.NotBlank;

@PropertiesEqual(propertyNameOne = "passwordOne", propertyNameTwo = "passwordTwo", message = "{passwords.do.not.match}")
public class ResetPasswordPair implements PasswordPair {
  @NotBlank(message = "{request.blank}")
  private String request;

  @PasswordPolicy
  @NotBlank(message = "{reset.password.blank}")
  private String passwordOne;
  private String passwordTwo;

  public String getRequest() {
    return request;
  }

  public void setRequest(String request) {
    this.request = request;
  }

  @Override
  public String getPasswordOne() {
    return passwordOne;
  }

  @Override
  public void setPasswordOne(String passwordOne) {
    this.passwordOne = passwordOne;
  }

  @Override
  public String getPasswordTwo() {
    return passwordTwo;
  }

  @Override
  public void setPasswordTwo(String passwordTwo) {
    this.passwordTwo = passwordTwo;
  }
}
