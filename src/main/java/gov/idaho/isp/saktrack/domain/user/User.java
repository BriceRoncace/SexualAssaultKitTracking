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

package gov.idaho.isp.saktrack.domain.user;

import org.springframework.security.core.userdetails.UserDetails;

public interface User extends UserDetails {
  public enum Type {
    ADMIN("admin", "admin"),
    LAB("lab", "lab"),
    MEDICAL("medical", "medical"),
    LAW_ENFORCEMENT("law-enforcement", "law-enforcement"),
    LEGAL("legal", "legal");

    private final String label;
    private final String devLabel;

    private Type(String label, String devLabel) {
      this.label = label;
      this.devLabel = devLabel;
    }

    public String getLabel() {
      return label;
    }

    public String getDevLabel() {
      return devLabel;
    }
  }

  Long getId();
  String getDisplayName();
  String getEmail();
  String getPhone();
  Type getType();
  boolean isAdmin();
  boolean isOrganizationAdmin();
  void setUserDetails(UserDetails userDetails);
}