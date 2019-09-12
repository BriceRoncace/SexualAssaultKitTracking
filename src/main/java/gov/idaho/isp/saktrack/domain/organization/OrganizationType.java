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

package gov.idaho.isp.saktrack.domain.organization;

import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.LabUser;
import gov.idaho.isp.saktrack.domain.user.organization.LawEnforcementUser;
import gov.idaho.isp.saktrack.domain.user.organization.LegalUser;
import gov.idaho.isp.saktrack.domain.user.organization.MedicalUser;

public enum OrganizationType {
  LAB("Lab") {
    @Override
    public AbstractOrganizationUser getNewUser() {
      return new LabUser();
    }
  },
  LAW_ENFORCEMENT("Law Enforcement") {
    @Override
    public AbstractOrganizationUser getNewUser() {
      return new LawEnforcementUser();
    }
  },
  MEDICAL("Medical") {
    @Override
    public AbstractOrganizationUser getNewUser() {
      return new MedicalUser();
    }
  },
  LEGAL("Legal") {
    @Override
    public AbstractOrganizationUser getNewUser() {
      return new LegalUser();
    }
  };

  private final String label;
  private final String devLabel;

  private OrganizationType(String label) {
    this.label = label;
    this.devLabel = getNewUser().getType().getDevLabel();
  }

  public abstract AbstractOrganizationUser getNewUser();

  public String getLabel() {
    return label;
  }

  public String getDevLabel() {
    return devLabel;
  }
}
