/*
 * Copyright (C) 2023 Czech Technical University in Prague
 *
 * This program is free software: you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation, either version 3 of the License, or (at your option) any
 * later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details. You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package cz.cvut.kbss.jopa.oom.metamodel;

import cz.cvut.kbss.jopa.model.metamodel.AbstractIdentifiableType;
import cz.cvut.kbss.jopa.model.metamodel.EntityType;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.Type;
import cz.cvut.kbss.jopa.oom.exceptions.AmbiguousEntityTypeException;
import cz.cvut.kbss.ontodriver.model.Axiom;
import cz.cvut.kbss.ontodriver.model.NamedResource;

import java.net.URI;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Collectors;

public class PolymorphicEntityTypeResolver<T> {

    private final NamedResource individual;
    private final Set<URI> types;
    private final IdentifiableEntityType<T> root;

    private final Set<IdentifiableEntityType<? extends T>> matches = new HashSet<>(2);

    public PolymorphicEntityTypeResolver(NamedResource individual, IdentifiableEntityType<T> root,
                                         Collection<Axiom<URI>> typeAxioms) {
        this.individual = individual;
        this.types = typeAxioms.stream().map(a -> a.getValue().getValue()).collect(Collectors.toSet());
        this.root = root;
    }

    /**
     * Returns entity type suitable for instance loading. This entity type is
     * <ul>
     * <li>either the specified {@code root} in case the type axioms contain type corresponding to the root entity
     * type,
     * <li>or the most specific non-abstract entity type from the hierarchy of the specified root entity type
     * present in the specified type axioms.
     * </ul>
     *
     * @return The specified root entity type or the most specific non-abstract unique entity type
     * @throws AmbiguousEntityTypeException When multiple entity types match the specified types
     */
    public IdentifiableEntityType<? extends T> determineActualEntityType() {
        if (types.contains(root.getIRI().toURI()) && !root.isAbstract()) {
            return root;
        }
        resolveMatchingEntityTypes();
        if (matches.size() > 1) {
            throw new AmbiguousEntityTypeException(
                    "Unable to determine unique entity type for loading individual " + individual +
                            ". Matching types are " + matches + '.');
        }
        return !matches.isEmpty() ? matches.iterator().next() : null;
    }

    /**
     * The algorithm uses DFS with remembering the path taken. If a matching entity type is found, but there already
     * exists a more general one (an ancestor of the ET), then the ancestor is removed from the matches, because it is
     * superseded by the more specific entity type just found.
     */
    private void resolveMatchingEntityTypes() {
        findMatchingEntityType(root, new HashSet<>());
    }

    private void findMatchingEntityType(AbstractIdentifiableType<? extends T> parent,
                                        Set<EntityType<? extends T>> ancestors) {
        for (AbstractIdentifiableType<? extends T> subtype : parent.getSubtypes()) {
            final Set<EntityType<? extends T>> updatedAncestors = new HashSet<>(ancestors);
            if (subtype.getPersistenceType() == Type.PersistenceType.ENTITY && !subtype.isAbstract()) {
                assert subtype instanceof IdentifiableEntityType;
                final IdentifiableEntityType<? extends T> et = (IdentifiableEntityType<? extends T>) subtype;
                final URI etUri = et.getIRI().toURI();
                if (types.contains(etUri)) {
                    addMatchingType(et, ancestors);
                }
                updatedAncestors.add(et);
            }
            findMatchingEntityType(subtype, updatedAncestors);
        }
    }

    private void addMatchingType(IdentifiableEntityType<? extends T> et, Set<EntityType<? extends T>> ancestors) {
        matches.add(et);
        ancestors.forEach(matches::remove);
    }
}
