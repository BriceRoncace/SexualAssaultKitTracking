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

package gov.idaho.isp.saktrack.mailer;

import jakarta.activation.DataSource;
import jakarta.mail.util.ByteArrayDataSource;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

public class MailMessage implements org.springframework.mail.MailMessage {
  private String from;
  private String envelopeFrom;
  private Collection<String> toEmails = new ArrayList<>();
  private Collection<String> ccEmails = new ArrayList<>();
  private Collection<String> bccEmails = new ArrayList<>();
  private String replyTo;
  private String subject;
  private String text;
  private LocalDateTime sent;
  private MimeType mimeType;
  private Collection<DataSource> attachments = new ArrayList<>();
  private Map<String,DataSource> inlineElements = new HashMap<>();

  @Override
  public void setTo(String to) {
    clearToEmails();
    toEmails.addAll(MailUtils.splitAddresses(to));
  }

  @Override
  public void setTo(String... toEmails) {
    setTo(Arrays.asList(toEmails));
  }

  public void addTo(String to) {
    toEmails.addAll(MailUtils.splitAddresses(to));
  }

  public void setTo(Collection<String> toEmails) {
    if (toEmails == null) {
      clearToEmails();
    }
    else {
      this.toEmails = new ArrayList<>(toEmails);
    }
  }

  public void clearToEmails() {
    toEmails.clear();
  }

  public Collection<String> getToEmails() {
    toEmails = MailUtils.clean(toEmails);
    return Collections.unmodifiableCollection(toEmails);
  }

  @Override
  public void setCc(String cc) {
    clearCcEmails();
    ccEmails.addAll(MailUtils.splitAddresses(cc));
  }

  @Override
  public void setCc(String... ccEmails) {
    setCc(Arrays.asList(ccEmails));
  }

  public void addCc(String cc) {
    ccEmails.addAll(MailUtils.splitAddresses(cc));
  }

  public void setCc(Collection<String> ccEmails) {
    if (ccEmails == null) {
      clearCcEmails();
    }
    else {
      this.ccEmails = new ArrayList<>(ccEmails);
    }
  }

  public void clearCcEmails() {
    ccEmails.clear();
  }

  public Collection<String> getCcEmails() {
    ccEmails = MailUtils.clean(ccEmails);
    return Collections.unmodifiableCollection(ccEmails);
  }

  @Override
  public void setBcc(String bcc) {
    clearBccEmails();
    bccEmails.addAll(MailUtils.splitAddresses(bcc));
  }

  @Override
  public void setBcc(String... bccEmails) {
    setBcc(Arrays.asList(bccEmails));
  }

  public void addBcc(String bcc) {
    bccEmails.addAll(MailUtils.splitAddresses(bcc));
  }

  public void setBcc(Collection<String> bccEmails) {
    if (bccEmails == null) {
      clearBccEmails();
    }
    else {
      this.bccEmails = new ArrayList<>(bccEmails);
    }
  }

  public void clearBccEmails() {
    bccEmails.clear();
  }

  public Collection<String> getBccEmails() {
    bccEmails = MailUtils.clean(bccEmails);
    return Collections.unmodifiableCollection(bccEmails);
  }

  public String getEnvelopeFrom() {
    return envelopeFrom;
  }

  /**
   * Set the From address to appear in the SMTP envelope. Note that this is different than the From address that appears in the message itself. The envelope From address is typically used when reporting errors. See RFC 821 for details. If set, overrides the mail.smtp.from property.
   */
  public void setEnvelopeFrom(String envelopeFrom) {
    this.envelopeFrom = envelopeFrom;
  }

  @Override
  public void setFrom(String from) {
    this.from = from;
  }

  public String getFrom() {
    return from;
  }

  public String getReplyTo() {
    return replyTo;
  }

  @Override
  public void setReplyTo(String replyTo) {
    this.replyTo = replyTo;
  }

  @Override
  public void setSubject(String subject) {
    this.subject = subject;
  }

  public String getSubject() {
    return subject;
  }

  @Override
  public void setText(String text) {
    this.text = text;
  }

  public String getText() {
    return text;
  }

  @Override
  public void setSentDate(Date date){
    setSent(Instant.ofEpochMilli(date.getTime()).atZone(ZoneId.systemDefault()).toLocalDateTime());
  }

  public void setSent(LocalDateTime sent) {
    this.sent = sent;
  }

  public LocalDateTime getSent() {
    return sent;
  }

  public Date getSentDate() {
    return sent != null ?  Date.from(sent.atZone(ZoneId.systemDefault()).toInstant()) : null;
  }

  /**
   * Tip: use javax.mail.util.ByteArrayDataSource
   */
  public void addAttachment(DataSource attachment) throws IllegalStateException {
    if (attachment != null && (attachment.getName() == null || "".equals(attachment.getName().trim()))) {
      throw new IllegalStateException("DataSource attachment name cannot be null.");
    }

    if (!this.attachments.contains(attachment)) {
      this.attachments.add(attachment);
    }
  }

  public void addAttachment(String name, byte[] content, String mimeType) throws IllegalStateException {
    ByteArrayDataSource ds = new ByteArrayDataSource(content, mimeType);
    ds.setName(name);
    addAttachment(ds);
  }

  public void setAttachments(Collection<DataSource> attachments) {
    if (attachments == null) {
      removeAllAttachments();
    }
    else {
      this.attachments = attachments;
    }
  }

  public void removeAllAttachments() {
    attachments.clear();
  }

  public Collection<DataSource> getAttachments() {
    return Collections.unmodifiableCollection(attachments);
  }

  public void addInlineElement(String contentId, DataSource dataSource) {
    inlineElements.put(contentId, dataSource);
  }

  /**
   * Map containing a contentId key and DataSource value
   */
  public void setInlineElements(Map<String,DataSource> inlineElements) {
    if (inlineElements == null) {
      removeAllInlineElements();
    }
    else {
      this.inlineElements = inlineElements;
    }
  }

  public void removeAllInlineElements() {
    inlineElements.clear();
  }

  public Map<String,DataSource> getInlineElements() {
    return Collections.unmodifiableMap(inlineElements);
  }

  public MimeType getMimeType() {
    return mimeType;
  }

  public void setMimeType(MimeType mimeType) {
    this.mimeType = mimeType;
  }

  @Override
  public String toString() {
    return "MailMessage{" + "from=" + from + ", envelopeFrom=" + envelopeFrom + ", toEmails=" + toEmails + ", ccEmails=" + ccEmails + ", bccEmails=" + bccEmails + ", replyTo=" + replyTo + ", subject=" + subject + ", text=" + text + ", sent=" + sent + ", mimeType=" + mimeType + ", attachments=" + attachments + ", inlineElements=" + inlineElements + '}';
  }
}