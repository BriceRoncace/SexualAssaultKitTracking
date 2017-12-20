package gov.idaho.isp.saktrack.user.persistence;

import gov.idaho.isp.saktrack.user.AbstractUser;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.repository.Repository;

public interface AbstractUserDirectory extends Repository<AbstractUser,Long>, JpaSpecificationExecutor<AbstractUser> {
}