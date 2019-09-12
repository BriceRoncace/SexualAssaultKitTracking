/* 
 * Copyright 2017 Idaho State Police.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package gov.idaho.isp.saktrack.domain.organization;

import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction_;
import java.util.ArrayList;
import java.util.List;
import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Join;
import javax.persistence.criteria.Predicate;
import javax.persistence.criteria.Root;
import org.apache.commons.lang3.StringUtils;
import org.springframework.data.jpa.domain.Specification;

public class OrganizationSpec implements Specification<Organization> {
  private final OrganizationSearchCriteria criteria;

  public OrganizationSpec(OrganizationSearchCriteria criteria) {
    this.criteria = criteria;
  }

  @Override
  public Predicate toPredicate(Root<Organization> root, CriteriaQuery<?> cq, CriteriaBuilder cb) {

    List<Predicate> predicates = new ArrayList<>();

    if (StringUtils.isNotBlank(criteria.getName())) {
      predicates.add(cb.like(cb.lower(root.get(Organization_.name)), StringUtils.lowerCase(criteria.getName()) + "%"));
    }

    if (criteria.getType() != null) {
      predicates.add(cb.equal(root.get(Organization_.type), criteria.getType()));
    }

    if (criteria.getJurisdictionId() != null) {
      Join<Organization, Jurisdiction> jurisdiction = root.join(Organization_.jurisdiction);
      predicates.add(cb.equal(jurisdiction.get(Jurisdiction_.id), criteria.getJurisdictionId()));
    }

    return andTogether(predicates, cb);
  }

  private Predicate andTogether(List<Predicate> predicates, CriteriaBuilder cb) {
    return cb.and(predicates.toArray(new Predicate[0]));
  }
}
