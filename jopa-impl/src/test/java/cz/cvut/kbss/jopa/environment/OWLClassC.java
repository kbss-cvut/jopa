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
package cz.cvut.kbss.jopa.environment;

import cz.cvut.kbss.jopa.environment.utils.HasUri;
import cz.cvut.kbss.jopa.model.annotations.FetchType;
import cz.cvut.kbss.jopa.model.annotations.Id;
import cz.cvut.kbss.jopa.model.annotations.OWLClass;
import cz.cvut.kbss.jopa.model.annotations.OWLObjectProperty;
import cz.cvut.kbss.jopa.model.annotations.RDFCollection;
import cz.cvut.kbss.jopa.model.annotations.RDFContainer;
import cz.cvut.kbss.jopa.model.annotations.RDFContainerType;
import cz.cvut.kbss.jopa.model.annotations.Sequence;
import cz.cvut.kbss.jopa.model.annotations.SequenceType;

import java.lang.reflect.Field;
import java.net.URI;
import java.util.List;

@OWLClass(iri = Vocabulary.c_OwlClassC)
public class OWLClassC implements HasUri {

    private static final String REF_LIST_FIELD = "referencedList";
    private static final String SIMPLE_LIST_FIELD = "simpleList";

    @Id
    private URI uri;

    @Sequence
    @OWLObjectProperty(iri = Vocabulary.P_HAS_REFERENCED_LIST, fetch = FetchType.EAGER)
    private List<OWLClassA> referencedList;

    @Sequence(type = SequenceType.simple)
    @OWLObjectProperty(iri = Vocabulary.P_HAS_SIMPLE_LIST)
    private List<OWLClassA> simpleList;

    @RDFCollection
    @OWLObjectProperty(iri = Vocabulary.P_HAS_RDF_COLLECTION)
    private List<OWLClassA> rdfCollection;

    @RDFContainer(type = RDFContainerType.SEQ)
    @OWLObjectProperty(iri = Vocabulary.P_HAS_RDF_SEQ, fetch = FetchType.EAGER)
    private List<OWLClassA> rdfSeq;

    public OWLClassC() {
    }

    public OWLClassC(URI uri) {
        this.uri = uri;
    }

    public void setUri(URI uri) {
        this.uri = uri;
    }

    public URI getUri() {
        return uri;
    }

    public void setReferencedList(List<OWLClassA> list) {
        this.referencedList = list;
    }

    public List<OWLClassA> getReferencedList() {
        return referencedList;
    }

    public void setSimpleList(List<OWLClassA> simpleList) {
        this.simpleList = simpleList;
    }

    public List<OWLClassA> getSimpleList() {
        return simpleList;
    }

    public List<OWLClassA> getRdfCollection() {
        return rdfCollection;
    }

    public void setRdfCollection(List<OWLClassA> rdfCollection) {
        this.rdfCollection = rdfCollection;
    }

    public List<OWLClassA> getRdfSeq() {
        return rdfSeq;
    }

    public void setRdfSeq(List<OWLClassA> rdfSeq) {
        this.rdfSeq = rdfSeq;
    }

    public static String getClassIri() {
        return OWLClassC.class.getAnnotation(OWLClass.class).iri();
    }

    public static Field getRefListField() throws NoSuchFieldException, SecurityException {
        return OWLClassC.class.getDeclaredField(REF_LIST_FIELD);
    }

    public static Field getSimpleListField() throws NoSuchFieldException, SecurityException {
        return OWLClassC.class.getDeclaredField(SIMPLE_LIST_FIELD);
    }

    public static Field getRdfCollectionField() throws NoSuchFieldException, SecurityException {
        return OWLClassC.class.getDeclaredField("rdfCollection");
    }

    public static Field getRdfSeqField() throws NoSuchFieldException, SecurityException {
        return OWLClassC.class.getDeclaredField("rdfSeq");
    }

    @Override
    public String toString() {
        String out = "OWLClassC: uri = " + uri;
        if (referencedList != null) {
            out += ", referencedList = {" + referencedList + "}";
        }
        if (simpleList != null) {
            out += ", simpleList = {" + simpleList + "}";
        }
        if (rdfSeq != null) {
            out += ", rdfSeq = {" + rdfSeq + "}";
        }
        return out;
    }
}
