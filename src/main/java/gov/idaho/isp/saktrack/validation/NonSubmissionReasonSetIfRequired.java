package gov.idaho.isp.saktrack.validation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import javax.validation.Constraint;
import javax.validation.Payload;

@Constraint(validatedBy = NonSubmissionReasonSetIfRequiredValidator.class)
@Target( { ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
public @interface NonSubmissionReasonSetIfRequired {
  public String message() default "{non.submission.reason.required}";
  public Class<?>[] groups() default {};
  public Class<? extends Payload>[] payload() default {};
}