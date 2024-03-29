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

package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.domain.ChainOfCustodyEvent.EventType;
import jakarta.validation.ConstraintValidator;
import jakarta.validation.ConstraintValidatorContext;

public class ChainOfCustodyEventValidator implements ConstraintValidator<ChainOfCustodyEventValid, ChainOfCustodyEvent> {
  @Override
  public void initialize(ChainOfCustodyEventValid constraintAnnotation) {
  }

  @Override
  public boolean isValid(ChainOfCustodyEvent event, ConstraintValidatorContext context) {
    if (event == null) {
      return true;
    }

    if (event.getEventType() == EventType.CREATE) {
      setMessage("{invalid.chain.of.custody.create}", context);
      return isValidForCreate(event);
    }
    else if (event.getEventType() == EventType.SEND) {
      setMessage("{invalid.chain.of.custody.send}", context);
      return isValidForSend(event);
    }
    else if (event.getEventType() == EventType.RECEIVE) {
      setMessage("{invalid.chain.of.custody.receive}", context);
      return isValidForReceive(event);
    }
    else if (event.getEventType() == EventType.REPURPOSE) {
      setMessage("{invalid.chain.of.custody.repurpose}", context);
      return isValidForRepurpose(event);
    }
    else if (event.getEventType() == EventType.DESTROY) {
      setMessage("{invalid.chain.of.custody.destroy}", context);
      return isValidForDestroy(event);
    }

    return true;
  }

  private void setMessage(String message, ConstraintValidatorContext context) {
    //disable existing violation message
    context.disableDefaultConstraintViolation();
    //build new violation message and add it
    context.buildConstraintViolationWithTemplate(message).addConstraintViolation();
  }

  private boolean isValidForCreate(ChainOfCustodyEvent event) {
    return hasToOrganization(event);
  }

  private boolean isValidForSend(ChainOfCustodyEvent event) {
    return hasToOrganization(event) && hasFromOrganization(event);
  }

  private boolean isValidForReceive(ChainOfCustodyEvent event) {
    return hasToOrganization(event) && hasFromOrganization(event);
  }

  private boolean isValidForRepurpose(ChainOfCustodyEvent event) {
    return hasFromOrganization(event);
  }

  private boolean isValidForDestroy(ChainOfCustodyEvent event) {
    return hasFromOrganization(event);
  }

  private boolean hasToOrganization(ChainOfCustodyEvent event) {
    return event.getTo() != null;
  }

  private boolean hasFromOrganization(ChainOfCustodyEvent event) {
    return event.getFrom() != null;
  }
}
