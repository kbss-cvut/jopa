package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.utils.NamespaceResolver;

/**
 * Context holding data when an {@link IdentifiableType} is being built.
 *
 * @param <X> Java type represented by the type
 */
class TypeBuilderContext<X> {

    private final AbstractIdentifiableType<X> type;

    private final NamespaceResolver namespaceResolver;

    TypeBuilderContext(AbstractIdentifiableType<X> type, NamespaceResolver namespaceResolver) {
        this.type = type;
        this.namespaceResolver = namespaceResolver;
    }

    void registerNamespace(String prefix, String namespace) {
        namespaceResolver.registerNamespace(prefix, namespace);
    }

    String resolveNamespace(String iri) {
        return namespaceResolver.resolveFullIri(iri);
    }

    AbstractIdentifiableType<X> getType() {
        return type;
    }
}
