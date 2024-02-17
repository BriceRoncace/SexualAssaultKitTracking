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

package gov.idaho.isp.saktrack.service;

import gov.idaho.isp.saktrack.domain.jurisdiction.Jurisdiction;
import gov.idaho.isp.saktrack.domain.jurisdiction.JurisdictionRepository;
import gov.idaho.isp.saktrack.domain.organization.Organization;
import gov.idaho.isp.saktrack.domain.organization.OrganizationRepository;
import gov.idaho.isp.saktrack.domain.organization.OrganizationType;
import gov.idaho.isp.saktrack.domain.user.AbstractUser;
import gov.idaho.isp.saktrack.domain.user.organization.AbstractOrganizationUser;
import gov.idaho.isp.saktrack.domain.user.organization.OrganizationUserRepository;
import gov.idaho.isp.saktrack.exception.SexualAssaultKitTrackingException;
import gov.idaho.isp.saktrack.util.PasswordUtil;
import gov.idaho.isp.saktrack.util.beanvalidation.BeanValidationUtils;
import jakarta.validation.ConstraintViolation;
import jakarta.validation.Validator;
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;
import org.apache.commons.lang3.StringUtils;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

@Service
public class CommonsDataImporterImpl implements DataImporter {
  private final OrganizationRepository organizationRepository;
  private final OrganizationUserRepository organizationUserRepository;
  private final JurisdictionRepository jurisdictionRepository;
  private final PasswordEncoder passwordEncoder;
  private final Validator validator;

  public CommonsDataImporterImpl(OrganizationRepository organizationRepository, OrganizationUserRepository organizationUserRepository, JurisdictionRepository jurisdictionRepository, PasswordEncoder passwordEncoder, Validator validator) {
    this.organizationRepository = organizationRepository;
    this.organizationUserRepository = organizationUserRepository;
    this.jurisdictionRepository = jurisdictionRepository;
    this.passwordEncoder = passwordEncoder;
    this.validator = validator;
  }

  @Override
  public Integer importNewOrganizations(MultipartFile csvFile) throws SexualAssaultKitTrackingException {
    Set<String> errors = new LinkedHashSet<>();
    List<Organization> orgs = new ArrayList<>();

    try (CSVParser parser = CSVParser.parse(new String(csvFile.getBytes(), StandardCharsets.UTF_8), CSVFormat.DEFAULT)) {
      for (CSVRecord record : parser) {
        try {
          Organization org = new Organization();
          org.setName(getStringValue(record.get(0), "Name"));
          org.setType(getOrganizationType(record.get(1)));
          org.setJurisdiction(getJurisdiction(record.get(2)));
          org.setPasskey(PasswordUtil.generatePasskey());
          org.setEnabled(true);
          validateOrganization(org);
          duplicateOrganizationCheck(org, orgs);
          orgs.add(org);
        }
        catch (RuntimeException ex) {
          errors.add("There was an error on line " + record.getRecordNumber() + ": " + ex.getMessage());
        }
      }
    }
    catch (IOException ex) {
      errors.add("There was an error reading the file, please try again.");
    }

    if (!errors.isEmpty()) {
      throw new SexualAssaultKitTrackingException("CSV Parser Errors", errors);
    }

    organizationRepository.saveAll(orgs);
    return orgs.size();
  }

  @Override
  public Integer importNewOrganizationUsers(MultipartFile csvFile) throws SexualAssaultKitTrackingException {
    Set<String> errors = new LinkedHashSet<>();
    List<AbstractOrganizationUser> users = new ArrayList<>();
    Map<String, Organization> organizationMap = new HashMap<>();

    try (CSVParser parser = CSVParser.parse(new String(csvFile.getBytes(), StandardCharsets.UTF_8), CSVFormat.DEFAULT)) {
      for (CSVRecord record : parser) {
        try {
          Organization org = getOrganizationValue(organizationMap, record.get(7));
          AbstractOrganizationUser user = org.getType().getNewUser();
          user.setOrganizationAdmin(getBooleanValue(record.get(0), "Organization Administrator"));
          user.setOrganizationContact(getBooleanValue(record.get(1), "Organization Contact"));
          user.setUsername(getStringValue(record.get(2), "Username"));
          user.setDisplayName(getStringValue(record.get(3), "Display Name"));
          user.setPhone(getStringValue(record.get(4), "Phone"));
          user.setEmail(getStringValue(record.get(5), "Email"));
          user.setPassword(getPasswordValue(record.get(6)));
          user.setOrganization(org);
          user.setPasskey(org.getPasskey());
          user.setVerifiedDate(LocalDate.now());
          user.setEnabled(true);
          validateUser(user);
          duplicateUserCheck(user, users);
          users.add(user);
        }
        catch (SexualAssaultKitTrackingException ex) {
          for (String s : ex.getErrors()) {
            errors.add("There was an error on line " + record.getRecordNumber() + ": " + s);
          }
        }
        catch (RuntimeException ex) {
          errors.add("There was an error on line " + record.getRecordNumber() + ": " + ex.getMessage());
        }
      }
    }
    catch (IOException ex) {
      errors.add("There was an error reading the file, please try again.");
    }

    if (!errors.isEmpty()) {
      throw new SexualAssaultKitTrackingException("CSV Parser Errors", errors);
    }

    organizationUserRepository.saveAll(users);
    return users.size();
  }

  private void duplicateUserCheck(AbstractOrganizationUser user, List<AbstractOrganizationUser> users) {
    List<String> names = users.stream().map(AbstractUser::getUsername).toList();
    if (names.contains(user.getUsername())) {
      throw new RuntimeException(user.getUsername() + " has already been used in this list.");
    }
  }

  private void duplicateOrganizationCheck(Organization org, List<Organization> orgs) {
    List<String> names = orgs.stream().map(Organization::getName).toList();
    if (names.contains(org.getName())) {
      throw new RuntimeException(org.getName() + " has already been used in this list.");
    }
  }

  private void validateUser (AbstractOrganizationUser user) {
    Set<ConstraintViolation<AbstractOrganizationUser>> violations = validator.validate(user);
    if (!violations.isEmpty()) {
      throw new SexualAssaultKitTrackingException("validation errors", BeanValidationUtils.getErrorMessages(violations));
    }
  }

  private void validateOrganization(Organization org) {
    Set<ConstraintViolation<Organization>> violations = validator.validate(org);
    if (!violations.isEmpty()) {
      throw new SexualAssaultKitTrackingException("validation errors", BeanValidationUtils.getErrorMessages(violations));
    }
  }

  private Organization getOrganizationValue(Map<String,Organization> orgMap, String token) {
    if (StringUtils.isNotBlank(token)) {
      if (orgMap.containsKey(token)) {
        return orgMap.get(token);
      }

      Organization org = organizationRepository.findByName(token);
      if (org != null) {
        orgMap.put(org.getName(), org);
        return org;
      }
      else {
        throw new RuntimeException("Could not find organization");
      }
    }
    throw new RuntimeException("Organization name is blank.");
  }

  private String getPasswordValue(String token) {
    if (StringUtils.isNotBlank(token)) {
      return passwordEncoder.encode(token.trim());
    }
    throw new RuntimeException("Password is blank.");
  }

  private String getStringValue(String token, String tokenName) throws RuntimeException {
    if (StringUtils.isNotBlank(token)) {
      return token.trim();
    }
    throw new RuntimeException(String.format("%s is blank", tokenName));
  }

  private boolean getBooleanValue(String token, String tokenName) throws RuntimeException {
    List<String> trueAnswers = Arrays.asList("1", "true", "yes");
    List<String> falseAnswers = Arrays.asList("0", "false", "no");
    if (trueAnswers.contains(StringUtils.lowerCase(token.trim()))) {
      return true;
    }
    else if (falseAnswers.contains(StringUtils.lowerCase(token.trim())) || StringUtils.isBlank(token)) {
      return false;
    }
    else {
      throw new RuntimeException(String.format("%s is an invalid True/False value", tokenName));
    }
  }

  private OrganizationType getOrganizationType(String token) throws RuntimeException {
    try {
      return OrganizationType.valueOf(token.trim().replace(" ", "_").toUpperCase());
    }
    catch (IllegalArgumentException | NullPointerException ex) {
      throw new RuntimeException("Invalid Organization Type");
    }
  }

  private Jurisdiction getJurisdiction(String token) throws RuntimeException {
    token = token != null ? token.trim() : "";
    Jurisdiction j = jurisdictionRepository.findByName(token);

    if (j != null) {
      return j;
    }
    else {
      throw new RuntimeException("Could not find jurisdiction");
    }
  }
}
