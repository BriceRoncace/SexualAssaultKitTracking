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

package gov.idaho.isp.saktrack.domain.user.organization;

import gov.idaho.isp.saktrack.domain.dto.EventDetails;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.exception.IllegalTransferException;
import java.io.Serializable;
import jakarta.persistence.DiscriminatorValue;
import jakarta.persistence.Entity;
import jakarta.persistence.Inheritance;
import jakarta.persistence.InheritanceType;
import org.springframework.beans.factory.config.ConfigurableBeanFactory;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Component;

@Entity
@DiscriminatorValue(value = "Medical")
@Inheritance(strategy = InheritanceType.SINGLE_TABLE)
@Component @Scope(ConfigurableBeanFactory.SCOPE_PROTOTYPE)
public class MedicalUser extends AbstractOrganizationUser implements Serializable {

  @Override
  public User.Type getType() {
    return User.Type.MEDICAL;
  }

  public void batchReceive(EventDetails eventDetails) {
    userKitService.receive(this, eventDetails);
  }

  public void batchSendToLawEnforcement(EventDetails eventDetails) {
    userKitService.send(this, eventDetails, OrganizationType.LAW_ENFORCEMENT);
  }

  public void batchSendToLab(EventDetails eventDetails) {
    userKitService.send(this, eventDetails, OrganizationType.LAB);
  }

  public void batchSendToMedical(EventDetails eventDetails) {
    userKitService.send(this, eventDetails, OrganizationType.MEDICAL);
  }

  public void repurpose(EventDetails eventDetails) {
    if (eventDetails.getSerialNumberList().size() > 1) {
      throw new IllegalTransferException("Only one kit can be repurposed at a time.");
    }

    userKitService.repurpose(this, eventDetails);
  }

  @Override
  public String toString() {
    return "MedicalUser{" + super.toString() + '}';
  }
}
