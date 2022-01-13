/**
 * Copyright (C) 2022 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.vocabulary;

/**
 * A subset of the Dublin Core Metadata Initiative vocabulary.
 * <p>
 * See <a href="http://dublincore.org/documents/dcmi-terms/">http://dublincore.org/documents/dcmi-terms/</a>
 */
public final class DC {

    private DC() {
        throw new AssertionError();
    }

    /**
     * Vocabulary in the /terms namespace of Dublin Core.
     */
    public static class Terms {

        /**
         * DC /terms namespace.
         */
        public static final String NAMESPACE = "http://purl.org/dc/terms/";

        /**
         * A summary of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/abstract">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/abstract</a>
         */
        public static final String ABSTRACT = NAMESPACE + "abstract";

        /**
         * Information about who access the resource or an indication of its security status.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/accessRights">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/accessRights</a>
         */
        public static final String ACCESS_RIGHTS = NAMESPACE + "accessRights";

        /**
         * The method by which items are added to a collection.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/accrualMethod">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/accrualMethod</a>
         */
        public static final String ACCRUAL_METHOD = NAMESPACE + "accrualMethod";

        /**
         * The frequency with which items are added to a collection.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/accrualPeriodicity">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/accrualPeriodicity</a>
         */
        public static final String ACCRUAL_PERIODICITY = NAMESPACE + "accrualPeriodicity";

        /**
         * The policy governing the addition of items to a collection.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/accrualPolicy">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/accrualPolicy</a>
         */
        public static final String ACCRUAL_POLICY = NAMESPACE + "accrualPolicy";

        /**
         * A resource that acts or has the power to act.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Agent">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Agent</a>
         */
        public static final String AGENT = NAMESPACE + "Agent";

        /**
         * A group of agents.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/AgentClass">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/AgentClass</a>
         */
        public static final String AGENT_CLASS = NAMESPACE + "AgentClass";

        /**
         * An alternative name for the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/alternative">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/alternative</a>
         */
        public static final String ALTERNATIVE = NAMESPACE + "alternative";

        /**
         * A class of agents for whom the resource is intended or useful.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/audience">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/audience</a>
         */
        public static final String AUDIENCE = NAMESPACE + "audience";

        /**
         * Date that the resource became or will become available.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/available">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/available</a>
         */
        public static final String AVAILABLE = NAMESPACE + "available";

        /**
         * A bibliographic reference for the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/bibliographicCitation">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/bibliographicCitation</a>
         */
        public static final String BIBLIOGRAPHIC_CITATION = NAMESPACE + "bibliographicCitation";

        /**
         * A book, article, or other documentary resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/BibliographicResource">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/BibliographicResource</a>
         */
        public static final String BIBLIOGRAPHIC_RESOURCE = NAMESPACE + "BibliographicResource";

        /**
         * An established standard to which the described resource conforms.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/conformsTo">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/conformsTo</a>
         */
        public static final String CONFORMS_TO = NAMESPACE + "conformsTo";

        /**
         * An entity responsible for making contributions to the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/contributor">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/contributor</a>
         */
        public static final String CONTRIBUTOR = NAMESPACE + "contributor";

        /**
         * The spatial or temporal topic of the resource, spatial applicability of the resource, or jurisdiction under which the resource is relevant.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/coverage">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/coverage</a>
         */
        public static final String COVERAGE = NAMESPACE + "coverage";

        /**
         * Date of creation of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/created">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/created</a>
         */
        public static final String CREATED = NAMESPACE + "created";

        /**
         * An entity primarily responsible for making the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/creator">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/creator</a>
         */
        public static final String CREATOR = NAMESPACE + "creator";

        /**
         * A point or period of time associated with an event in the lifecycle of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/date">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/date</a>
         */
        public static final String DATE = NAMESPACE + "date";

        /**
         * Date of acceptance of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/dateAccepted">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/dateAccepted</a>
         */
        public static final String DATE_ACCEPTED = NAMESPACE + "dateAccepted";

        /**
         * Date of copyright of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/dateCopyrighted">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/dateCopyrighted</a>
         */
        public static final String DATE_COPYRIGHTED = NAMESPACE + "dateCopyrighted";

        /**
         * Date of submission of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/dateSubmitted">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/dateSubmitted</a>
         */
        public static final String DATE_SUBMITTED = NAMESPACE + "dateSubmitted";

        /**
         * An account of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/description">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/description</a>
         */
        public static final String DESCRIPTION = NAMESPACE + "description";

        /**
         * A class of agents, defined in terms of progression through an educational or training context, for which the described resource is intended.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/educationLevel">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/educationLevel</a>
         */
        public static final String EDUCATION_LEVEL = NAMESPACE + "educationLevel";

        /**
         * The size or duration of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/extent">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/extent</a>
         */
        public static final String EXTENT = NAMESPACE + "extent";

        /**
         * A digital resource format.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/FileFormat">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/FileFormat</a>
         */
        public static final String FILE_FORMAT = NAMESPACE + "FileFormat";

        /**
         * The file format, physical medium, or dimensions of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/format">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/format</a>
         */
        public static final String FORMAT = NAMESPACE + "format";

        /**
         * A rate at which something recurs.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Frequency">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Frequency</a>
         */
        public static final String FREQUENCY = NAMESPACE + "Frequency";

        /**
         * A related resource that is substantially the same as the pre-existing described resource, but in another format.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/hasFormat">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/hasFormat</a>
         */
        public static final String HAS_FORMAT = NAMESPACE + "hasFormat";

        /**
         * A related resource that is included either physically or logically in the described resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/hasPart">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/hasPart</a>
         */
        public static final String HAS_PART = NAMESPACE + "hasPart";

        /**
         * A related resource that is a version, edition, or adaptation of the described resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/hasVersion">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/hasVersion</a>
         */
        public static final String HAS_VERSION = NAMESPACE + "hasVersion";

        /**
         * An unambiguous reference to the resource within a given context.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/identifier">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/identifier</a>
         */
        public static final String IDENTIFIER = NAMESPACE + "identifier";

        /**
         * A process, used to engender knowledge, attitudes and skills, that the described resource is designed to support.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/instructionalMethod">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/instructionalMethod</a>
         */
        public static final String INSTRUCTIONAL_METHOD = NAMESPACE + "instructionalMethod";

        /**
         * A pre-existing related resource that is substantially the same as the described resource, but in another format.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isFormatOf">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isFormatOf</a>
         */
        public static final String IS_FORMAT_OF = NAMESPACE + "isFormatOf";

        /**
         * A related resource in which the described resource is physically or logically included.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isPartOf">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isPartOf</a>
         */
        public static final String IS_PART_OF = NAMESPACE + "isPartOf";

        /**
         * A related resource that references, cites, or otherwise points to the described resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isReferencedBy">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isReferencedBy</a>
         */
        public static final String IS_REFERENCED_BY = NAMESPACE + "isReferencedBy";

        /**
         * A related resource that supplants, displaces, or supersedes the described resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isReplacedBy">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isReplacedBy</a>
         */
        public static final String IS_REPLACED_BY = NAMESPACE + "isReplacedBy";

        /**
         * A related resource that requires the described resource to support its function, delivery, or coherence.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isRequiredBy">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isRequiredBy</a>
         */
        public static final String IS_REQUIRED_BY = NAMESPACE + "isRequiredBy";

        /**
         * Date of formal issuance of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/issued">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/issued</a>
         */
        public static final String ISSUED = NAMESPACE + "issued";

        /**
         * A related resource of which the described resource is a version, edition, or adaptation.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isVersionOf">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/isVersionOf</a>
         */
        public static final String IS_VERSION_OF = NAMESPACE + "isVersionOf";

        /**
         * The extent or range of judicial, law enforcement, or other authority.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Jurisdiction">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Jurisdiction</a>
         */
        public static final String JURISDICTION = NAMESPACE + "Jurisdiction";

        /**
         * A language of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/language">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/language</a>
         */
        public static final String LANGUAGE = NAMESPACE + "language";

        /**
         * A legal document giving official permission to do something with the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/license">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/license</a>
         */
        public static final String LICENSE = NAMESPACE + "license";

        /**
         * A legal document giving official permission to do something with a resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/LicenseDocument">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/LicenseDocument</a>
         */
        public static final String LICENSE_DOCUMENT = NAMESPACE + "LicenseDocument";

        /**
         * A system of signs, symbols, sounds, gestures, or rules used in communication.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/LinguisticSystem">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/LinguisticSystem</a>
         */
        public static final String LINGUISTIC_SYSTEM = NAMESPACE + "LinguisticSystem";

        /**
         * A spatial region or named place.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Location">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Location</a>
         */
        public static final String LOCATION = NAMESPACE + "Location";

        /**
         * A location, period of time, or jurisdiction.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/LocationPeriodOrJurisdiction">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/LocationPeriodOrJurisdiction</a>
         */
        public static final String LOCATION_PERIOD_OR_JURISDICTION = NAMESPACE + "LocationPeriodOrJurisdiction";

        /**
         * An entity that mediates access to the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/mediator">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/mediator</a>
         */
        public static final String MEDIATOR = NAMESPACE + "mediator";

        /**
         * A file format or physical medium.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/MediaType">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/MediaType</a>
         */
        public static final String MEDIA_TYPE = NAMESPACE + "MediaType";

        /**
         * A media type or extent.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/MediaTypeOrExtent">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/MediaTypeOrExtent</a>
         */
        public static final String MEDIA_TYPE_OR_EXTENT = NAMESPACE + "MediaTypeOrExtent";

        /**
         * The material or physical carrier of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/medium">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/medium</a>
         */
        public static final String MEDIUM = NAMESPACE + "medium";

        /**
         * A method by which resources are added to a collection.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/MethodOfAccrual">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/MethodOfAccrual</a>
         */
        public static final String METHOD_OF_ACCRUAL = NAMESPACE + "MethodOfAccrual";

        /**
         * A process that is used to engender knowledge, attitudes, and skills.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/MethodOfInstruction">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/MethodOfInstruction</a>
         */
        public static final String METHOD_OF_INSTRUCTION = NAMESPACE + "MethodOfInstruction";

        /**
         * Date on which the resource was changed.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/modified">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/modified</a>
         */
        public static final String MODIFIED = NAMESPACE + "modified";

        /**
         * An interval of time that is named or defined by its start and end dates.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/PeriodOfTime">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/PeriodOfTime</a>
         */
        public static final String PERIOD_OF_TYPE = NAMESPACE + "PeriodOfTime";

        /**
         * A physical material or carrier.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/PhysicalMedium">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/PhysicalMedium</a>
         */
        public static final String PHYSICAL_MEDIUM = NAMESPACE + "PhysicalMedium";

        /**
         * A material thing.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/PhysicalResource">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/PhysicalResource</a>
         */
        public static final String PHYSICAL_RESOURCE = NAMESPACE + "PhysicalResource";

        /**
         * A plan or course of action by an authority, intended to influence and determine decisions, actions, and other matters.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Policy">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Policy</a>
         */
        public static final String POLICY = NAMESPACE + "Policy";

        /**
         * A statement of any changes in ownership and custody of the resource since its creation that are significant for its authenticity, integrity, and interpretation.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/provenance">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/provenance</a>
         */
        public static final String PROVENANCE = NAMESPACE + "provenance";

        /**
         * Any changes in ownership and custody of a resource since its creation that are significant for its authenticity, integrity, and interpretation.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/ProvenanceStatement">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/ProvenanceStatement</a>
         */
        public static final String PROVENANCE_STATEMENT = NAMESPACE + "ProvenanceStatement";

        /**
         * An entity responsible for making the resource available.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/publisher">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/publisher</a>
         */
        public static final String PUBLISHER = NAMESPACE + "publisher";

        /**
         * A related resource that is referenced, cited, or otherwise pointed to by the described resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/references">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/references</a>
         */
        public static final String REFERENCES = NAMESPACE + "references";

        /**
         * A related resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/relation">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/relation</a>
         */
        public static final String RELATION = NAMESPACE + "relation";

        /**
         * A related resource that is supplanted, displaced, or superseded by the described resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/replaces">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/replaces</a>
         */
        public static final String REPLACES = NAMESPACE + "replaces";

        /**
         * A related resource that is required by the described resource to support its function, delivery, or coherence.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/requires">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/requires</a>
         */
        public static final String REQUIRES = NAMESPACE + "requires";

        /**
         * Information about rights held in and over the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/rights">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/rights</a>
         */
        public static final String RIGHTS = NAMESPACE + "rights";

        /**
         * A person or organization owning or managing rights over the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/rightsHolder">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/rightsHolder</a>
         */
        public static final String RIGHTS_HOLDER = NAMESPACE + "rightsHolder";

        /**
         * A statement about the intellectual property rights (IPR) held in or over a resource, a legal document giving official permission to do something with a resource, or a statement about access rights.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/RightsStatement">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/RightsStatement</a>
         */
        public static final String RIGHTS_STATEMENT = NAMESPACE + "RightsStatement";

        /**
         * A dimension or extent, or a time taken to play or execute.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/SizeOrDuration">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/SizeOrDuration</a>
         */
        public static final String SIZE_OR_DURATION = NAMESPACE + "SizeOrDuration";

        /**
         * A related resource from which the described resource is derived.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/source">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/source</a>
         */
        public static final String SOURCE = NAMESPACE + "source";

        /**
         * Spatial characteristics of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/spatial">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/spatial</a>
         */
        public static final String SPATIAL = NAMESPACE + "spatial";

        /**
         * A reference point against which other things can be evaluated or compared.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Standard">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/Standard</a>
         */
        public static final String STANDARD = NAMESPACE + "Standard";

        /**
         * The topic of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/subject">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/subject</a>
         */
        public static final String SUBJECT = NAMESPACE + "subject";

        /**
         * A list of subunits of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/tableOfContents">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/tableOfContents</a>
         */
        public static final String TABLE_OF_CONTENTS = NAMESPACE + "tableOfContents";

        /**
         * Temporal characteristics of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/temporal">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/temporal</a>
         */
        public static final String TEMPORAL = NAMESPACE + "temporal";

        /**
         * A name given to the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/title">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/title</a>
         */
        public static final String TITLE = NAMESPACE + "title";

        /**
         * The nature or genre of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/type">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/type</a>
         */
        public static final String TYPE = NAMESPACE + "type";

        /**
         * Date (often a range) of validity of a resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/valid">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/terms/valid</a>
         */
        public static final String VALID = NAMESPACE + "valid";

        private Terms() {
            throw new AssertionError();
        }
    }

    /**
     * Vocabulary in the /elements/1.1/ namespace of Dublin Core.
     */
    public static class Elements {

        /**
         * DC /elements/1.1/ namespace.
         */
        public static final String NAMESPACE = "http://purl.org/dc/elements/1.1/";

        /**
         * An entity primarily responsible for making the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/creator">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/creator</a>
         */
        public static final String CREATOR = NAMESPACE + "creator";

        /**
         * A point or period of time associated with an event in the lifecycle of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/date">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/date</a>
         */
        public static final String DATE = NAMESPACE + "date";

        /**
         * An account of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/description">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/description</a>
         */
        public static final String DESCRIPTION = NAMESPACE + "description";

        /**
         * An unambiguous reference to the resource within a given context.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/identifier">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/identifier</a>
         */
        public static final String IDENTIFIER = NAMESPACE + "identifier";

        /**
         * A related resource from which the described resource is derived.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/source">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/source</a>
         */
        public static final String SOURCE = NAMESPACE + "source";

        /**
         * The topic of the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/subject">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/subject</a>
         */
        public static final String SUBJECT = NAMESPACE + "subject";

        /**
         * A name given to the resource.
         *
         * @see <a href="https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/title">https://www.dublincore.org/specifications/dublin-core/dcmi-terms/#http://purl.org/dc/elements/1.1/title</a>
         */
        public static final String TITLE = NAMESPACE + "title";

        private Elements() {
            throw new AssertionError();
        }
    }
}
