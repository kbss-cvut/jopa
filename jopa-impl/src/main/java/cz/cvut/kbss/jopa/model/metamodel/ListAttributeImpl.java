/**
 * Copyright (C) 2016 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.AttributeConverter;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;

import java.lang.reflect.Field;
import java.util.List;

public class ListAttributeImpl<X, V> extends PluralAttributeImpl<X, List<V>, V>
        implements ListAttribute<X, V> {

    private final IRI owlListClass;

    private final IRI owlObjectPropertyHasNext;

    private final IRI owlPropertyHasContents;

    private final SequenceType owlSequenceType;

    private ListAttributeImpl(ListAttributeBuilder<X, V> builder) {
        super(builder);
        this.owlListClass = builder.owlListClass;
        this.owlObjectPropertyHasNext = builder.owlObjectPropertyHasNext;
        this.owlPropertyHasContents = builder.owlPropertyHasContents;
        this.owlSequenceType = builder.owlSequenceType;
    }

    @Override
    public CollectionType getCollectionType() {
        return CollectionType.LIST;
    }

    @Override
    public IRI getOWLListClass() {
        return owlListClass;
    }

    @Override
    public IRI getOWLObjectPropertyHasNextIRI() {
        return owlObjectPropertyHasNext;
    }

    @Override
    public IRI getOWLPropertyHasContentsIRI() {
        return owlPropertyHasContents;
    }

    @Override
    public SequenceType getSequenceType() {
        return owlSequenceType;
    }

    @Override
    public String toString() {
        return "ListAttribute[" + getName() + "]";
    }

    public static ListAttributeBuilder iri(IRI iri) {
        return new ListAttributeBuilder().collectionType(List.class).iri(iri);
    }

    public static class ListAttributeBuilder<X, V> extends PluralAttributeBuilder<X, List<V>, V> {
        private IRI owlListClass;
        private IRI owlObjectPropertyHasNext;
        private IRI owlPropertyHasContents;
        private SequenceType owlSequenceType;

        @Override
        public ListAttributeBuilder<X, V> elementType(Type<V> elementType) {
            super.elementType(elementType);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> collectionType(Class<List<V>> collectionType) {
            super.collectionType(collectionType);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> field(Field field) {
            super.field(field);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> declaringType(ManagedType<X> declaringType) {
            super.declaringType(declaringType);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> attributeType(PersistentAttributeType attributeType) {
            super.attributeType(attributeType);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> iri(IRI iri) {
            super.iri(iri);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> cascadeTypes(CascadeType[] cascadeTypes) {
            super.cascadeTypes(cascadeTypes);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> fetchType(FetchType fetchType) {
            super.fetchType(fetchType);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> inferred(boolean inferred) {
            super.inferred(inferred);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> includeExplicit(boolean includeExplicit) {
            super.includeExplicit(includeExplicit);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> participationConstraints(ParticipationConstraint[] constraints) {
            super.participationConstraints(constraints);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> name(String name) {
            super.name(name);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> constraints(ParticipationConstraint[] constraints) {
            super.constraints(constraints);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> nonEmpty(boolean nonEmpty) {
            super.nonEmpty(nonEmpty);
            return this;
        }

        @Override
        public ListAttributeBuilder<X, V> converter(AttributeConverter<List<V>, ?> converter) {
            super.converter(converter);
            return this;
        }

        public ListAttributeBuilder<X, V> owlListClass(IRI owlListClass) {
            this.owlListClass = owlListClass;
            return this;
        }

        public ListAttributeBuilder<X, V> hasNextProperty(IRI hasNextProperty) {
            this.owlObjectPropertyHasNext = hasNextProperty;
            return this;
        }

        public ListAttributeBuilder<X, V> hasContentsProperty(IRI hasContentsProperty) {
            this.owlPropertyHasContents = hasContentsProperty;
            return this;
        }

        public ListAttributeBuilder<X, V> sequenceType(SequenceType sequenceType) {
            this.owlSequenceType = sequenceType;
            return this;
        }

        @Override
        public ListAttributeImpl<X, V> build() {
            return new ListAttributeImpl<>(this);
        }
    }
}
