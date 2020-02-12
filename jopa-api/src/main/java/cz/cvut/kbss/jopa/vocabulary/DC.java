/**
 * Copyright (C) 2020 Czech Technical University in Prague
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
public class DC {

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
         * Date of creation of the resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-created">http://dublincore.org/documents/dcmi-terms/#terms-created</a>
         */
        public static final String CREATED = NAMESPACE + "created";

        /**
         * An entity primarily responsible for making the resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-creator">http://dublincore.org/documents/dcmi-terms/#terms-creator</a>
         */
        public static final String CREATOR = NAMESPACE + "creator";

        /**
         * A point or period of time associated with an event in the lifecycle of the resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-date">http://dublincore.org/documents/dcmi-terms/#terms-date</a>
         */
        public static final String DATE = NAMESPACE + "date";

        /**
         * An account of the resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-description">http://dublincore.org/documents/dcmi-terms/#terms-description</a>
         */
        public static final String DESCRIPTION = NAMESPACE + "description";

        /**
         * A related resource that is included either physically or logically in the described resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-hasPart">http://dublincore.org/documents/dcmi-terms/#terms-hasPart</a>
         */
        public static final String HAS_PART = NAMESPACE + "hasPart";

        /**
         * A related resource that is a version, edition, or adaptation of the described resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-hasVersion">http://dublincore.org/documents/dcmi-terms/#terms-hasVersion</a>
         */
        public static final String HAS_VERSION = NAMESPACE + "hasVersion";

        /**
         * An unambiguous reference to the resource within a given context.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-identifier">http://dublincore.org/documents/dcmi-terms/#terms-identifier</a>
         */
        public static final String IDENTIFIER = NAMESPACE + "identifier";

        /**
         * A related resource in which the described resource is physically or logically included.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-isPartOf">http://dublincore.org/documents/dcmi-terms/#terms-isPartOf</a>
         */
        public static final String IS_PART_OF = NAMESPACE + "isPartOf";

        /**
         * A related resource that references, cites, or otherwise points to the described resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-isPartOf">http://dublincore.org/documents/dcmi-terms/#terms-isPartOf</a>
         */
        public static final String IS_REFERENCED_BY = NAMESPACE + "isReferencedBy";

        /**
         * A related resource from which the described resource is derived.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-source">http://dublincore.org/documents/dcmi-terms/#terms-source</a>
         */
        public static final String SOURCE = NAMESPACE + "source";

        /**
         * The topic of the resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-subject">http://dublincore.org/documents/dcmi-terms/#terms-subject</a>
         */
        public static final String SUBJECT = NAMESPACE + "subject";

        /**
         * A name given to the resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#terms-title">http://dublincore.org/documents/dcmi-terms/#terms-title</a>
         */
        public static final String TITLE = NAMESPACE + "title";

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
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#elements-creator">http://dublincore.org/documents/dcmi-terms/#elements-creator</a>
         */
        public static final String CREATOR = NAMESPACE + "creator";

        /**
         * A point or period of time associated with an event in the lifecycle of the resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#elements-date">http://dublincore.org/documents/dcmi-terms/#elements-date</a>
         */
        public static final String DATE = NAMESPACE + "date";

        /**
         * An account of the resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#elements-description">http://dublincore.org/documents/dcmi-terms/#elements-description</a>
         */
        public static final String DESCRIPTION = NAMESPACE + "description";

        /**
         * An unambiguous reference to the resource within a given context.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#elements-identifier">http://dublincore.org/documents/dcmi-terms/#elements-identifier</a>
         */
        public static final String IDENTIFIER = NAMESPACE + "identifier";

        /**
         * A related resource from which the described resource is derived.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#elements-source">http://dublincore.org/documents/dcmi-terms/#elements-source</a>
         */
        public static final String SOURCE = NAMESPACE + "source";

        /**
         * The topic of the resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#elements-subject">http://dublincore.org/documents/dcmi-terms/#elements-subject</a>
         */
        public static final String SUBJECT = NAMESPACE + "subject";

        /**
         * A name given to the resource.
         *
         * @see <a href="http://dublincore.org/documents/dcmi-terms/#elements-title">http://dublincore.org/documents/dcmi-terms/#elements-title</a>
         */
        public static final String TITLE = NAMESPACE + "title";

        private Elements() {
            throw new AssertionError();
        }
    }
}
