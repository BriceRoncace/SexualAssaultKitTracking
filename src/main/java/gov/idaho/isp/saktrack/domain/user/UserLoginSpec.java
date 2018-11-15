package gov.idaho.isp.saktrack.domain.user;

import gov.idaho.isp.saktrack.domain.search.CriteriaDate;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.jpa.domain.Specification;

public class UserLoginSpec implements Specification<UserLogin> {
  private Long organizationId;
  private String displayName;
  private String username;
  private String email;
  private CriteriaDate loginDate = new CriteriaDate();

  @Override
  public Predicate toPredicate(Root<UserLogin> root, CriteriaQuery<?> cq, CriteriaBuilder cb) {
    List<Predicate> predicates = new ArrayList<>();

    if (organizationId != null) {
      predicates.add(cb.equal(root.get(UserLogin_.organizationId), organizationId));
    }

    if (StringUtils.isNotBlank(displayName)) {
      predicates.add(cb.like(cb.lower(root.get(UserLogin_.displayName)), StringUtils.lowerCase(displayName) + "%"));
    }

    if (StringUtils.isNotBlank(username)) {
      predicates.add(cb.like(cb.lower(root.get(UserLogin_.username)), StringUtils.lowerCase(username) + "%"));
    }

    if (StringUtils.isNotBlank(email)) {
      predicates.add(cb.like(cb.lower(root.get(UserLogin_.email)), StringUtils.lowerCase(email) + "%"));
    }

    if (loginDate.canBuildPredicate()) {
      predicates.add(loginDate.buildPredicate(cb, root.get(UserLogin_.LOGIN_TIME)));
    }

    return andTogether(predicates, cb);
  }

  private Predicate andTogether(List<Predicate> predicates, CriteriaBuilder cb) {
    return cb.and(predicates.toArray(new Predicate[0]));
  }

  public Long getOrganizationId() {
    return organizationId;
  }

  public void setOrganizationId(Long organizationId) {
    this.organizationId = organizationId;
  }

  public String getDisplayName() {
    return displayName;
  }

  public void setDisplayName(String displayName) {
    this.displayName = displayName;
  }

  public String getUsername() {
    return username;
  }

  public void setUsername(String username) {
    this.username = username;
  }

  public String getEmail() {
    return email;
  }

  public void setEmail(String email) {
    this.email = email;
  }

  public CriteriaDate getLoginDate() {
    return loginDate;
  }

  public void setLoginDate(CriteriaDate loginDate) {
    this.loginDate = loginDate;
  }

  @Override
  public String toString() {
    return "UserLoginSpec{" + "organizationId=" + organizationId + ", displayName=" + displayName + ", username=" + username + ", email=" + email + ", loginDate=" + loginDate + '}';
  }
}
