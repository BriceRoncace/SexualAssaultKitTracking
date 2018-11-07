package gov.idaho.isp.saktrack.domain.user.password;

import java.time.LocalDateTime;
import java.util.List;
import org.springframework.data.jpa.repository.JpaRepository;

public interface PasswordTokenRepository extends JpaRepository<PasswordToken, Long> {
  PasswordToken findByToken(String token);
  List<PasswordToken> findByUserId(Long id);
  List<PasswordToken> findByGenerationDateBefore(LocalDateTime generationDate);
}
