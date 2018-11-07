package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.service.email.EmailService;
import gov.idaho.isp.saktrack.domain.user.AbstractUser;
import gov.idaho.isp.saktrack.domain.user.AbstractUserRepository;
import gov.idaho.isp.saktrack.domain.user.User;
import gov.idaho.isp.saktrack.domain.user.password.PasswordToken;
import gov.idaho.isp.saktrack.domain.user.password.PasswordTokenRepository;
import gov.idaho.isp.saktrack.domain.user.password.dto.ResetPasswordPair;
import java.time.LocalDateTime;
import java.util.Random;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

@Service
public class PasswordResetServiceImpl implements PasswordResetService {
  private final PasswordTokenRepository passwordTokenRepository;
  private final EmailService emailService;
  private final AbstractUserRepository abstractUserRepository;
  private final PasswordEncoder passwordEncoder;

  public PasswordResetServiceImpl(PasswordTokenRepository passwordTokenRepository, EmailService emailService, AbstractUserRepository abstractUserRepository, PasswordEncoder passwordEncoder) {
    this.passwordTokenRepository = passwordTokenRepository;
    this.emailService = emailService;
    this.abstractUserRepository = abstractUserRepository;
    this.passwordEncoder = passwordEncoder;
  }

  @Override
  public String requestReset(String username) {
    User user = abstractUserRepository.findByUsernameIgnoreCase(username);
    if (user == null) {
      return "password.reset.user.null";
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

    AbstractUser user = abstractUserRepository.findById(passwordToken.getUserId()).orElse(null);
    user.setPassword(passwordEncoder.encode(passwordPair.getPasswordOne()));
    abstractUserRepository.save(user);
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
}
