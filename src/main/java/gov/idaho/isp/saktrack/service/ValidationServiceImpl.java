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

package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.LabDetails;
import gov.idaho.isp.saktrack.domain.LawEnforcementDetails;
import gov.idaho.isp.saktrack.domain.LegalDetails;
import gov.idaho.isp.saktrack.domain.MedicalDetails;
import gov.idaho.isp.saktrack.domain.SexualAssaultKit;
import gov.idaho.isp.saktrack.exception.NullSexualAssaultKitException;
import java.util.Set;
import java.util.stream.Collectors;
import javax.validation.ConstraintViolation;
import javax.validation.Validator;
import org.springframework.stereotype.Service;

@Service
public class ValidationServiceImpl implements ValidationService {
  private final Validator validator;
  private final MessageService messageService;

  public ValidationServiceImpl(Validator validator, MessageService messageService) {
    this.validator = validator;
    this.messageService = messageService;
  }

  @Override
  public Set<String> validateKit(SexualAssaultKit kit, Class... groups) {
    if (kit == null) {
      throw new NullSexualAssaultKitException("Cannot validate missing sexual assault kit.");
    }
    return validate(kit, groups);
  }

  @Override
  public Set<String> validateMedicalDetails(MedicalDetails details, Class... groups) {
    if (details == null) {
      throw new NullSexualAssaultKitException("Cannot validate missing medical details.");
    }
    return validate(details, groups);
  }

  @Override
  public Set<String> validateLawEnforcementDetails(LawEnforcementDetails details, Class... groups) {
    if (details == null) {
      throw new NullSexualAssaultKitException("Cannot validate missing law enforcement details.");
    }
    return validate(details, groups);
  }

  @Override
  public Set<String> validateLegalDetails(LegalDetails details, Class... groups) {
    if (details == null) {
      throw new NullSexualAssaultKitException("Cannot validate missing legal details.");
    }
    return validate(details, groups);
  }

  @Override
  public Set<String> validateLabDetails(LabDetails details, Class... groups) {
    if (details == null) {
      throw new NullSexualAssaultKitException("Cannot validate missing lab details.");
    }
    return validate(details, groups);
  }

  @Override
  public Set<String> valiateChainOfCustodyEvent(ChainOfCustodyEvent event) {
    if (event == null) {
      throw new NullSexualAssaultKitException("Cannot validate missing chain of custody event.");
    }
    return validate(event);
  }

  private <T> Set<String> validate(T objectToValidate, Class... groups) {
    Set<ConstraintViolation<T>> constraintViolations = validator.validate(objectToValidate, groups);
    return asErrors(constraintViolations);
  }

  private Set<String> asErrors(Set<? extends ConstraintViolation> violations) {
    return violations.stream().map(this::constraintViolaionToErrorString).collect(Collectors.toSet());
  }

  private String constraintViolaionToErrorString(ConstraintViolation cv) {
    return messageService.getMessageText(cv.getMessage());
  }
}