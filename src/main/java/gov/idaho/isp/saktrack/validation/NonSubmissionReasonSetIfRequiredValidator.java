package gov.idaho.isp.saktrack.validation;

import gov.idaho.isp.saktrack.LawEnforcementDetails;
import javax.validation.ConstraintValidator;
import javax.validation.ConstraintValidatorContext;

public class NonSubmissionReasonSetIfRequiredValidator implements ConstraintValidator<NonSubmissionReasonSetIfRequired, LawEnforcementDetails> {

  @Override
  public void initialize(NonSubmissionReasonSetIfRequired constraintAnnotation) {
  }

  @Override
  public boolean isValid(LawEnforcementDetails leDetails, ConstraintValidatorContext context) {
    if (leDetails == null) {
      return true;
    }

    if (Boolean.FALSE.equals(leDetails.getMeetsSubmissionCriteria())) {
      return leDetails.getNonSubmissionReason() != null;
    }

    return true;
  }
}
