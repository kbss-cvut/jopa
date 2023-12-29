/*
 * JOPA
 * Copyright (C) 2023 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.vocabulary.RDF;
import cz.cvut.kbss.jopa.vocabulary.RDFS;
import cz.cvut.kbss.jopa.vocabulary.XSD;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Holds mapping of prefixes to namespaces and allows resolution of prefixed IRIs.
 * <p>
 * Prefixes for RDF (rdf:), RDFS (rdfs:) and XSD (xsd:) are pre-registered.
 */
public class NamespaceResolver {

    private static final Logger LOG = LoggerFactory.getLogger(NamespaceResolver.class);

    public static final String URN_PREFIX = "urn";

    private final Map<String, String> namespaces = new HashMap<>();

    public NamespaceResolver() {
        registerDefaultPrefixes();
    }

    private void registerDefaultPrefixes() {
        registerNamespace(RDF.PREFIX, RDF.NAMESPACE);
        registerNamespace(RDFS.PREFIX, RDFS.NAMESPACE);
        registerNamespace(XSD.PREFIX, XSD.NAMESPACE);
    }

    /**
     * Registers the specified namespace with the specified prefix.
     *
     * @param prefix    Prefix representing the namespace
     * @param namespace Namespace to represent
     */
    public void registerNamespace(String prefix, String namespace) {
        Objects.requireNonNull(prefix);
        Objects.requireNonNull(namespace);
        namespaces.put(prefix, namespace);
    }

    /**
     * Replaces prefix in the specified IRI with a full namespace IRI, if the IRI contains a prefix and it is registered
     * in this resolver.
     *
     * @param iri The IRI to resolve
     * @return Full IRI, if this resolver was able to resolve it, or the original IRI
     */
    public String resolveFullIri(String iri) {
        Objects.requireNonNull(iri);
        final int colonIndex = iri.indexOf(':');
        if (colonIndex == -1) {
            return iri;
        }
        if (iri.charAt(colonIndex + 1) == '/') {
            // iri is hierarchical
            return iri;
        }
        final String prefix = iri.substring(0, colonIndex);
        if (URN_PREFIX.equalsIgnoreCase(prefix)) {
            return iri;
        }
        if (!namespaces.containsKey(prefix)) {
            LOG.warn("Namespace for prefix '{}' not registered. Prefixed IRI '{}' will not be resolved.", prefix, iri);
            return iri;
        }
        final String localName = iri.substring(colonIndex + 1);
        return namespaces.get(prefix) + localName;
    }
}
