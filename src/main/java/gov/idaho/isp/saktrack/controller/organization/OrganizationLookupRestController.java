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

package gov.idaho.isp.saktrack.controller.organization;

import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class OrganizationLookupRestController {
  private final OrganizationRepository organizationRepository;

  public OrganizationLookupRestController(OrganizationRepository organizationRepository) {
    this.organizationRepository = organizationRepository;
  }

  @GetMapping("/organizations")
  public List<Organization> findOrganizationsByType(@RequestParam(name = "type") OrganizationType... types) {
    return types != null ? findByType(types) : Collections.emptyList();
  }

  private List<Organization> findByType(OrganizationType... types) {
    return Arrays.stream(types)
        .map(organizationRepository::findByTypeOrderByNameAsc)
        .flatMap(l -> l.stream())
        .collect(Collectors.toList());
  }
}