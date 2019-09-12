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

import gov.idaho.isp.saktrack.validation.OldPasswordPresentAndValid;

@OldPasswordPresentAndValid
public class ChangePasswordRequest extends SimplePasswordPair {
  private Long userId;
  private String oldPassword;

  public Long getUserId() {
    return userId;
  }

  public void setUserId(Long userId) {
    this.userId = userId;
  }

  public String getOldPassword() {
    return oldPassword;
  }

  public void setOldPassword(String oldPassword) {
    this.oldPassword = oldPassword;
  }

  @Override
  public String toString() {
    return "ChangePasswordRequest{" + "userId=" + userId + ", oldPassword=" + oldPassword + ", super=" + super.toString() + '}';
  }
}