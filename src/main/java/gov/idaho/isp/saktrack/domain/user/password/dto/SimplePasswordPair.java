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

@PropertiesEqual(propertyNameOne = "passwordOne", propertyNameTwo = "passwordTwo", message = "{passwords.do.not.match}")
public class SimplePasswordPair implements PasswordPair {
  @PasswordPolicy
  private String passwordOne;

  private String passwordTwo;

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

  @Override
  public String toString() {
    return "SimplePasswordPair{" + "passwordOne=" + passwordOne + ", passwordTwo=" + passwordTwo + '}';
  }
}
