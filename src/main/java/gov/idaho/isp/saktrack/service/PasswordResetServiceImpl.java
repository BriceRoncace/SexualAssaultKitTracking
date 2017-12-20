package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.user.User.AuthMethod;
import gov.idaho.isp.saktrack.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.user.organization.OrganizationUser;
import gov.idaho.isp.saktrack.user.password.PasswordToken;
import gov.idaho.isp.saktrack.user.password.PasswordTokenRepository;
import gov.idaho.isp.saktrack.user.password.dto.ResetPasswordPair;
import gov.idaho.isp.saktrack.user.persistence.OrganizationUserRepository;
import java.time.LocalDateTime;
import java.util.Random;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Service
public class PasswordResetServiceImpl implements PasswordResetService {
  private PasswordTokenRepository passwordTokenRepository;
  private EmailService emailService;
  private OrganizationUserRepository organizationUserRepository;
  private PasswordEncoder passwordEncoder;

  @Override
  public String requestReset(String username) {
    OrganizationUser user = organizationUserRepository.findByUsernameIgnoreCase(username);
    if (user == null) {
      return "password.reset.user.null";
    }
    else if (user.getAuthMethod() != AuthMethod.DATABASE) {
      return "password.reset.user.invalid.auth.method";
    }


    PasswordToken token = new PasswordToken(generateToken(40), user.getId());
    if (!emailService.sendPasswordResetEmail(token, user)) {
      return "password.reset.email.fail";
    }
    passwordTokenRepository.save(token);
    return null;
  }

  @Override
  public boolean isRequestValid(String request) {
    passwordTokenRepository.deleteInBatch(passwordTokenRepository.findByGenerationDateBefore(LocalDateTime.now().minusHours(24)));
    PasswordToken passwordToken = passwordTokenRepository.findByToken(request);
    return passwordToken != null;
  }

  @Override
  public boolean resetPassword(ResetPasswordPair passwordPair) {
    passwordTokenRepository.deleteInBatch(passwordTokenRepository.findByGenerationDateBefore(LocalDateTime.now().minusHours(24)));
    PasswordToken passwordToken = passwordTokenRepository.findByToken(passwordPair.getRequest());
    if (passwordToken == null) {
      return false;
    }

    AbstractOrganizationUser user = organizationUserRepository.findOne(passwordToken.getUserId());
    user.setPassword(passwordEncoder.encode(passwordPair.getPasswordOne()));
    organizationUserRepository.save(user);
    passwordTokenRepository.deleteInBatch(passwordTokenRepository.findByUserId(passwordToken.getUserId()));
    return true;
  }

  private String generateToken(int length) {
    final String CHARS = "ABCDEFGHIJKLMabcdefghijklm01234-_~56789nopqrstuvwxyzNOPQRSTUVWXYZ";
    Random random = new Random();
    StringBuilder token = new StringBuilder();
    for (int i = 0; i < length; i++) {
      token.append(CHARS.charAt(random.nextInt(CHARS.length())));
    }
    return token.toString();
  }

  @Autowired
  public void setPasswordTokenRepository(PasswordTokenRepository passwordTokenRepository) {
    this.passwordTokenRepository = passwordTokenRepository;
  }

  @Autowired
  public void setEmailService(EmailService emailService) {
    this.emailService = emailService;
  }

  @Autowired
  public void setOrganizationUserRepository(OrganizationUserRepository organizationUserRepository) {
    this.organizationUserRepository = organizationUserRepository;
  }

  @Autowired
  public void setPasswordEncoder(PasswordEncoder passwordEncoder) {
    this.passwordEncoder = passwordEncoder;
  }
}
