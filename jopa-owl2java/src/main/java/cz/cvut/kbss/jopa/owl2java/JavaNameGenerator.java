/*
 * JOPA
 * Copyright (C) 2025 Czech Technical University in Prague
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library.
 */
package cz.cvut.kbss.jopa.owl2java;

import cz.cvut.kbss.jopa.owl2java.prefix.PrefixMap;
import org.semanticweb.owlapi.model.IRI;
import org.semanticweb.owlapi.model.OWLOntologyID;

import java.text.Normalizer;
import java.util.Arrays;
import java.util.Objects;
import java.util.Optional;

/**
 * Generates Java names based on IRI identifiers.
 */
public class JavaNameGenerator {

    private static final String[] JAVA_KEYWORDS = {"abstract", "assert", "boolean", "break", "byte", "case", "catch", "char", "class", "const", "continue", "default", "do", "double", "else", "enum", "extends", "final", "finally", "float", "for", "goto", "if", "implements", "import", "instanceof", "int", "interface", "long", "native", "new", "package", "private", "protected", "public", "return", "short", "static", "strictfp", "super", "switch", "synchronized", "this", "throw", "throws", "transient", "try", "void", "volatile", "while"};

    private static final char SEPARATOR = '_';

    private final PrefixMap prefixMap;

    public JavaNameGenerator(PrefixMap prefixMap) {this.prefixMap = prefixMap;}

    /**
     * Returns a valid Java identifier extracted from the specified IRI.
     * <p>
     * If the IRI contains a non-empty fragment, it is used. Otherwise, the part after the last slash is used as the
     * name.
     *
     * @param iri IRI to extract name from
     * @return Java name based on the specified IRI
     */
    public String generateJavaNameForIri(IRI iri) {
        if (iri.getFragment() != null && !iri.getFragment().isEmpty()) {
            return makeNameValidJava(iri.getFragment());
        } else {
            String strIri = iri.toString();
            if (strIri.charAt(strIri.length() - 1) == '/') {
                strIri = strIri.substring(0, strIri.length() - 1);
            }
            int x = strIri.lastIndexOf("/");
            return makeNameValidJava(strIri.substring(x + 1));
        }
    }

    /**
     * Returns a valid Java identifier extracted from the specified IRI, prefixed with prefix registered for the
     * specified ontology IRI (if available).
     * <p>
     * If the IRI contains a non-empty fragment, it is used. Otherwise, the part after the last slash is used as the
     * name.
     * <p>
     * If the ontology is anonymous, no prefix is added to the extracted name. If no prefix is registered for the
     * ontology, a prefix represented by extracting a java name from the ontology IRI is used.
     *
     * @param iri        IRI to extract name from
     * @param ontologyId Ontology identifier
     * @return Java name based on the specified IRI
     */
    public String generatePrefixedJavaNameForIri(IRI iri, OWLOntologyID ontologyId) {
        if (ontologyId.isAnonymous()) {
            return generateJavaNameForIri(iri);
        }
        assert ontologyId.getOntologyIRI().isPresent();
        final IRI ontologyIri = ontologyId.getOntologyIRI().get();
        return makeNameValidJava(prefixMap.getPrefix(ontologyIri)
                                          .orElse(generateJavaNameForIri(ontologyIri))) + SEPARATOR + generateJavaNameForIri(iri);
    }

    /**
     * Gets the prefix registered for an ontology with the specified identifier.
     *
     * @param ontologyIri Ontology IRI
     * @return Optional ontology prefix
     */
    public Optional<String> getOntologyPrefix(IRI ontologyIri) {
        Objects.requireNonNull(ontologyIri);
        return prefixMap.getPrefix(ontologyIri);
    }

    /**
     * Checks whether a prefix exists for the specified ontology identifier.
     *
     * @param ontologyIri Ontology IRI
     * @return {@code true} if a prefix has been resolved for ontology IRI, {@code false} otherwise
     */
    public boolean hasPrefix(IRI ontologyIri) {
        return prefixMap.hasPrefix(ontologyIri);
    }

    /**
     * Returns the specified name sanitized for Java.
     * <p>
     * This means the result of this function can be used as/in a Java variable/field/class name.
     *
     * @param name The name to sanitize
     * @return Valid Java identifier
     */
    public static String makeNameValidJava(String name) {
        String res = name.trim().replace('-', SEPARATOR).replace("'", "_quote_")
                         .replace(".", "_dot_").replace(',', '_')
                         .replace("#", "");
        // Replace non-ASCII characters with ASCII ones
        res = Normalizer.normalize(res, Normalizer.Form.NFD).replaceAll("[^\\p{ASCII}]", "");
        if (Arrays.binarySearch(JAVA_KEYWORDS, res) >= 0) {
            res = SEPARATOR + res;
        }
        return res;
    }

    /**
     * Converts the specified name to the Java camel case notation.
     * <p>
     * This process removes underscores used to generate the name.
     *
     * @param name Generated name
     * @return Converted camel case name
     */
    public static String toCamelCaseNotation(String name) {
        StringBuilder result = new StringBuilder();
        for (String w : name.split(Character.toString(SEPARATOR))) {
            if (!w.isEmpty()) {
                result.append(w.substring(0, 1).toUpperCase()).append(w.substring(1));
            }
        }
        return result.toString();
    }
}
