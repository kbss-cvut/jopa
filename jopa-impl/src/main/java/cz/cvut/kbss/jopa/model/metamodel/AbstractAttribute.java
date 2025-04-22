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
package cz.cvut.kbss.jopa.model.metamodel;

import cz.cvut.kbss.jopa.model.IRI;
import cz.cvut.kbss.jopa.model.annotations.CascadeType;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.ParticipationConstraint;
import cz.cvut.kbss.jopa.oom.converter.ConverterWrapper;
import cz.cvut.kbss.ontodriver.model.InferenceMode;

import java.lang.reflect.Field;
import java.lang.reflect.Member;

public abstract class AbstractAttribute<X, Y> implements Attribute<X, Y> {

    private final PropertyInfo propertyInfo;

    private final ManagedType<X> declaringType;

    private final PersistentAttributeType attributeType;

    private final IRI iri;

    private final CascadeType[] cascadeTypes;

    private final FetchType fetchType;

    private final InferenceMode inferenceMode;

    private final boolean nonEmpty;

    private final boolean lexicalForm;

    private final boolean simpleLiteral;

    private final String datatype;

    private final String language;

    private final ParticipationConstraint[] constraints;

    private final ConverterWrapper converter;

    AbstractAttribute(AbstractAttributeBuilder<X, Y> builder) {
        this.propertyInfo = builder.propertyInfo;
        this.declaringType = builder.declaringType;
        this.attributeType = builder.attributeType;
        this.iri = builder.iri;
        this.cascadeTypes = builder.cascadeTypes;
        this.fetchType = builder.fetchType;
        this.inferenceMode = builder.inferenceMode;
        this.constraints = builder.constraints;
        this.nonEmpty = builder.nonEmpty;
        this.converter = builder.converter;
        this.lexicalForm = builder.lexicalForm;
        this.simpleLiteral = builder.simpleLiteral;
        this.language = builder.language;
        this.datatype = builder.datatype;
    }

    @Override
    public PersistentAttributeType getPersistentAttributeType() {
        return attributeType;
    }

    @Override
    public Member getJavaMember() {
        return propertyInfo.getMember();
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
        return propertyInfo.field();
    }

    @Override
    public FetchType getFetchType() {
        return fetchType;
    }

    @Override
    public boolean isInferred() {
        return inferenceMode != InferenceMode.EXPLICIT;
    }

    @Override
    public boolean includeExplicit() {
        return inferenceMode != InferenceMode.INFERRED;
    }

    @Override
    public boolean isLexicalForm() {
        return lexicalForm;
    }

    @Override
    public boolean isSimpleLiteral() {
        return simpleLiteral;
    }

    @Override
    public boolean hasLanguage() {
        return language != null && !simpleLiteral && !lexicalForm && attributeType != PersistentAttributeType.OBJECT;
    }

    @Override
    public String getLanguage() {
        return language;
    }

    @Override
    public String getName() {
        return propertyInfo.getName();
    }

    public ConverterWrapper getConverter() {
        return converter;
    }

    @Override
    public String toString() {
        return declaringType.getJavaType().getSimpleName() + "." + getName();
    }

    @Override
    public String getDatatype() {
        return datatype;
    }

    abstract static class AbstractAttributeBuilder<X, Y> {
        private PropertyInfo propertyInfo;
        private ManagedType<X> declaringType;
        private PersistentAttributeType attributeType;
        private IRI iri;
        private CascadeType[] cascadeTypes;
        private FetchType fetchType;
        private boolean inferred;
        private boolean includeExplicit;
        private InferenceMode inferenceMode;
        private boolean nonEmpty = false;
        private boolean lexicalForm = false;
        private boolean simpleLiteral = false;
        private String datatype;
        private String language;
        private ParticipationConstraint[] constraints;
        private ConverterWrapper converter;

        FieldMappingValidator mappingValidator;

        public AbstractAttributeBuilder<X, Y> config(PropertyAttributes config) {
            this.iri = config.getIri();
            this.attributeType = config.getPersistentAttributeType();
            this.cascadeTypes = config.getCascadeTypes();
            this.constraints = config.getParticipationConstraints();
            this.nonEmpty = config.isNonEmpty();
            this.fetchType = config.getFetchType();
            this.lexicalForm = config.isLexicalForm();
            this.simpleLiteral = config.isSimpleLiteral();
            this.datatype = config.hasDatatype() ? config.getDatatype() : null;
            this.language = config.getLanguage();
            this.mappingValidator = config.validator;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> propertyInfo(PropertyInfo propertyInfo) {
            this.propertyInfo = propertyInfo;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> declaringType(ManagedType<X> declaringType) {
            this.declaringType = declaringType;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> inferenceMode(InferenceMode inferenceMode) {
            this.inferenceMode = inferenceMode;
            return this;
        }

        public AbstractAttributeBuilder<X, Y> converter(ConverterWrapper converter) {
            this.converter = converter;
            return this;
        }

        public abstract AbstractAttribute<X, Y> build();
    }
}
