package cz.cvut.kbss.jopa.utils;

import cz.cvut.kbss.jopa.CommonVocabulary;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

/**
 * Holds mapping of prefixes to namespaces and allows resolution of prefixed IRIs.
 * <p>
 * Prefixes for RDF (rdf:), RDFS (rdfs:) and XSD (xsd:) are pre-registered.
 */
public class NamespaceResolver {

    private final Map<String, String> namespaces = new HashMap<>();

    public NamespaceResolver() {
        registerDefaultPrefixes();
    }

    private void registerDefaultPrefixes() {
        registerNamespace("rdf", CommonVocabulary.RDF_NAMESPACE);
        registerNamespace("rdfs", CommonVocabulary.RDFS_NAMESPACE);
        registerNamespace("xsd", CommonVocabulary.XSD_NAMESPACE);
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
     * Replaces prefix in the specified IRI with a full namespace IRI, if the IRI contains a prefix and it is registered in this resolver.
     *
     * @param iri The IRI to resolve
     * @return Full IRI, if this resolver was able to resolve it, or the original IRI
     */
    public String resolveFullIri(String iri) {
        Objects.requireNonNull(iri);
        final int colonIndex = iri.indexOf(':');
        if (colonIndex == -1 || colonIndex > iri.length()) {
            return iri;
        }
        final String prefix = iri.substring(0, colonIndex);
        if (!namespaces.containsKey(prefix)) {
            return iri;
        }
        final String localName = iri.substring(colonIndex + 1);
        return namespaces.get(prefix) + localName;
    }
}
