package gov.idaho.isp.saktrack.validation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import javax.validation.Constraint;
import javax.validation.Payload;


@Constraint(validatedBy = PasswordPolicyValidator.class)
@Target( { ElementType.FIELD, ElementType.METHOD })
@Retention(RetentionPolicy.RUNTIME)
public @interface PasswordPolicy {
  String message() default "{invalid.password}";
  Class<?>[] groups() default {};
  Class<? extends Payload>[] payload() default {};
  int capitals() default 0;
  int numbers() default 0;
  int specials() default 0;
  int minSize() default 0;
  int maxSize() default 2147483647;
}