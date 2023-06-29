/**
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.owl2java;

public class Constants {

    /**
     * Annotation property marking IC axioms.
     */
    public static final String P_IS_INTEGRITY_CONSTRAINT_FOR =
            "http://krizik.felk.cvut.cz/ontologies/2009/ic.owl#isIntegrityConstraintFor";

    /**
     * Annotation property specifying the name of Java class mapped by the annotated ontological class.
     */
    public static final String P_CLASS_NAME = "http://krizik.felk.cvut.cz/ontologies/2009/ic.owl#javaClassName";

    /**
     * Name of the class containing generated vocabulary.
     */
    public static final String VOCABULARY_CLASS = "Vocabulary";

    /**
     * Package into which the model is generated.
     */
    public static final String MODEL_PACKAGE = "model";

    /**
     * Java package name separator.
     */
    public static final char PACKAGE_SEPARATOR = '.';

    /**
     * Default language for language-tagged literals.
     */
    public static final String LANGUAGE = "en";

    /**
     * Name of the field representing entity identifier.
     */
    public static final String ID_FIELD_NAME = "id";

    /**
     * Name of the field representing the {@link cz.cvut.kbss.jopa.vocabulary.RDFS#LABEL} property.
     */
    public static final String LABEL_FIELD_NAME = "name";

    /**
     * Name of the field representing the {@link cz.cvut.kbss.jopa.vocabulary.DC.Terms#DESCRIPTION} property.
     */
    public static final String DESCRIPTION_FIELD_NAME = "description";

    /**
     * Name of the field representing {@link cz.cvut.kbss.jopa.model.annotations.Types}.
     */
    public static final String TYPES_FIELD_NAME = "types";

    /**
     * Name of the field representing {@link cz.cvut.kbss.jopa.model.annotations.Properties}.
     */
    public static final String PROPERTIES_FIELD_NAME = "properties";

    /**
     * Tool version.
     */
    public static final String VERSION = "$VERSION$";


    private Constants() {
        throw new AssertionError();
    }
}
