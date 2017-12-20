package gov.idaho.isp.saktrack.validation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import javax.validation.Constraint;
import javax.validation.Payload;

@Constraint(validatedBy = PasswordPresentIfRequiredValidator.class)
@Target( { ElementType.TYPE })
@Retention(RetentionPolicy.RUNTIME)
public @interface PasswordPresentIfRequired {
  public String message() default "{password.required}";
  public Class<?>[] groups() default {};
  public Class<? extends Payload>[] payload() default {};
}
