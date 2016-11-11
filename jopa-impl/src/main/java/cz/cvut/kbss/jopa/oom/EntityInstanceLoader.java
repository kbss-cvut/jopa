package cz.cvut.kbss.jopa.oom;

import cz.cvut.kbss.jopa.model.metamodel.*;
import cz.cvut.kbss.jopa.oom.exceptions.AmbiguousEntityTypeException;
import cz.cvut.kbss.jopa.sessions.LoadingParameters;
import cz.cvut.kbss.ontodriver.Connection;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Root of the entity loading strategies.
 */
abstract class EntityInstanceLoader {

    final Connection storageConnection;
    final Metamodel metamodel;

    final AxiomDescriptorFactory descriptorFactory;
    final EntityConstructor entityBuilder;

    EntityInstanceLoader(Connection storageConnection, Metamodel metamodel, AxiomDescriptorFactory descriptorFactory,
                         EntityConstructor entityBuilder) {
        this.storageConnection = storageConnection;
        this.metamodel = metamodel;
        this.descriptorFactory = descriptorFactory;
        this.entityBuilder = entityBuilder;
    }

    /**
     * Loads entity based on the specified loading parameters.
     *
     * @param loadingParameters Instance loading parameters
     * @return The loaded instance (possibly {@code null})
     */
    abstract <T> T loadEntity(LoadingParameters<T> loadingParameters);

    /**
     * Returns entity type suitable for instance loading. This entity type is
     * <pre>
     * <ul>
     *     <li>either the specified {@code rootEt} in case the type axioms contain type corresponding to the root entity
     * type,</li>
     *     <li>or the most specific non-abstract entity type from the hierarchy of the specified root entity type
     * present in the specified type axioms.</li>
     * </ul>
     * </pre>
     *
     * @param rootEt     Entity type search root
     * @param typeAxioms Collection of types of an individual
     * @return The specified root entity type or the most specific non-abstract unique entity type
     * @throws AmbiguousEntityTypeException When multiple entity types match the specified types
     */
    <T> EntityType<? extends T> determineActualEntityType(NamedResource individual, EntityTypeImpl<T> rootEt,
                                                          Collection<Axiom<URI>> typeAxioms) {
        final Set<URI> typeUris = typeAxioms.stream().map(a -> a.getValue().getValue()).collect(Collectors.toSet());
        if (typeUris.contains(rootEt.getIRI().toURI())) {
            return rootEt;
        }
        final Set<EntityType<? extends T>> matchingEt = new HashSet<>(2);
        resolveMatchingEntityTypes(rootEt, typeUris, matchingEt);
        if (matchingEt.size() > 1) {
            throw new AmbiguousEntityTypeException(
                    "Unable to determine unique entity type for loading individual " + individual +
                            ". Matching types are " + matchingEt + '.');
        }
        return !matchingEt.isEmpty() ? matchingEt.iterator().next() : null;
    }

    private <T> void resolveMatchingEntityTypes(AbstractIdentifiableType<? extends T> root, Set<URI> typeUris,
                                                Set<EntityType<? extends T>> matchingEts) {
        for (AbstractIdentifiableType<? extends T> subtype : root.getSubtypes()) {
            if (subtype.getPersistenceType() == Type.PersistenceType.ENTITY) {
                final EntityTypeImpl<? extends T> et = (EntityTypeImpl<? extends T>) subtype;
                final URI etUri = et.getIRI().toURI();
                if (typeUris.contains(etUri)) {
                    // TODO This is not enough, it has to work for all ancestors of the type, not just its direct parent
                    matchingEts.remove(et.getSupertype());
                    matchingEts.add(et);
                }
            }
            resolveMatchingEntityTypes(subtype, typeUris, matchingEts);
        }
    }
}
