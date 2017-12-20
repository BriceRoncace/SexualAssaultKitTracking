package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.ChainOfCustodyEvent;
import gov.idaho.isp.saktrack.LabDetails;
import gov.idaho.isp.saktrack.LawEnforcementDetails;
import gov.idaho.isp.saktrack.LegalDetails;
import gov.idaho.isp.saktrack.MedicalDetails;
import gov.idaho.isp.saktrack.SexualAssaultKit;
import gov.idaho.isp.saktrack.exception.NullSexualAssaultKitException;
import java.util.Set;
import java.util.stream.Collectors;
import javax.validation.ConstraintViolation;
import javax.validation.Validator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service
public class ValidationServiceImpl implements ValidationService {
  private Validator validator;
  private MessageService messageService;

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

  @Autowired
  public void setValidator(Validator validator) {
    this.validator = validator;
  }

  @Autowired
  public void setMessageService(MessageService messageService) {
    this.messageService = messageService;
  }
}