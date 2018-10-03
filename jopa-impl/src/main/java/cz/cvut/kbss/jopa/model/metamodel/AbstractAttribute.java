package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.AttributeConverter;
import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;

import java.lang.reflect.Field;
import java.lang.reflect.Member;

public abstract class AbstractAttribute<X, Y> implements Attribute<X, Y> {

    private final Field field;

    private final ManagedType<X> declaringType;

    private final PersistentAttributeType attributeType;

    private final IRI iri;

    private final CascadeType[] cascadeTypes;

    private final FetchType fetchType;

    private final boolean inferred;

    private final boolean includeExplicit;

    private final boolean nonEmpty;

    private final ParticipationConstraint[] constraints;

    private AttributeConverter<Y, ?> converter;

    AbstractAttribute(AbstractAttributeBuilder<X, Y> builder) {
        this.field = builder.field;
        this.declaringType = builder.declaringType;
        this.attributeType = builder.attributeType;
        this.iri = builder.iri;
        this.cascadeTypes = builder.cascadeTypes;
        this.fetchType = builder.fetchType;
        this.inferred = builder.inferred;
        this.includeExplicit = builder.includeExplicit;
        this.constraints = builder.constraints;
        this.nonEmpty = builder.nonEmpty;
        this.converter = builder.converter;
    }

    @Override
    public PersistentAttributeType getPersistentAttributeType() {
        return attributeType;
    }

    @Override
    public Member getJavaMember() {
        return field;
    }

    @Override
    public IRI getIRI() {
        return iri;
    }

    @Override
    public CascadeType[] getCascadeTypes() {
        return cascadeTypes;
    }

    @Override
    public boolean isNonEmpty() {
        return nonEmpty;
    }

    @Override
    public ParticipationConstraint[] getConstraints() {
        return constraints;
    }

    @Override
    public ManagedType<X> getDeclaringType() {
        return declaringType;
    }

    @Override
    public Field getJavaField() {
        return field;
    }

    @Override
    public FetchType getFetchType() {
        return fetchType;
    }

    @Override
    public boolean isInferred() {
        return inferred;
    }

    @Override
    public boolean includeExplicit() {
        return includeExplicit;
    }

    @Override
    public String getName() {
        return field.getName();
    }

    public AttributeConverter getConverter() {
        return converter;
    }

    abstract static class AbstractAttributeBuilder<X, Y> {
        private Field field;
        private ManagedType<X> declaringType;
        private PersistentAttributeType attributeType;
        private IRI iri;
        private CascadeType[] cascadeTypes;
        private FetchType fetchType;
        private boolean inferred;
        private boolean includeExplicit;
        private boolean nonEmpty = false;
        private ParticipationConstraint[] constraints;
        private AttributeConverter<Y, ?> converter;

        public AbstractAttributeBuilder<X, Y> field(Field field) {
            this.field = field;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> declaringType(ManagedType<X> declaringType) {
            this.declaringType = declaringType;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> attributeType(PersistentAttributeType attributeType) {
            this.attributeType = attributeType;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> iri(IRI iri) {
            this.iri = iri;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> cascadeTypes(CascadeType[] cascadeTypes) {
            this.cascadeTypes = cascadeTypes;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> fetchType(FetchType fetchType) {
            this.fetchType = fetchType;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> inferred(boolean inferred) {
            this.inferred = inferred;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> includeExplicit(boolean includeExplicit) {
            this.includeExplicit = includeExplicit;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> constraints(ParticipationConstraint[] constraints) {
            this.constraints = constraints;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> nonEmpty(boolean nonEmpty) {
            this.nonEmpty = nonEmpty;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> converter(AttributeConverter<Y, ?> converter) {
            this.converter = converter;
            return this;
        }
    }
}
