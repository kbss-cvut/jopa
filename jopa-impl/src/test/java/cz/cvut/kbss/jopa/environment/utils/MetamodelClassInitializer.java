/*
 * JOPA
 * Copyright (C) 2024 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.model.metamodel.*;

import java.util.List;
import java.util.Map;


public class MetamodelClassInitializer {

    private MetamodelClassInitializer() {
    }

    public static void initMetamodelClassOWLClassA(SingularAttribute<OWLClassA, String> stringAttribute, TypesSpecification<OWLClassA, String> types, Identifier uri) {
        OWLClassA_.stringAttribute = stringAttribute;
        OWLClassA_.types = types;
        OWLClassA_.uri = uri;
    }

    public static void initMetamodelClassOWLClassB(SingularAttribute<OWLClassB, String> stringAttribute, PropertiesSpecification<OWLClassB, Map, String, String> properties, Identifier uri) {
        OWLClassB_.stringAttribute = stringAttribute;
        OWLClassB_.properties = properties;
        OWLClassB_.uri = uri;
    }

    public static void initMetamodelClassOWLClassC(ListAttributeImpl<OWLClassC, OWLClassA> simpleList, ListAttributeImpl<OWLClassC, OWLClassA> referencedList, RDFContainerAttribute<OWLClassC, List<OWLClassA>, OWLClassA> rdfSeq, Identifier uri) {
        OWLClassC_.simpleList = simpleList;
        OWLClassC_.referencedList = referencedList;
        OWLClassC_.rdfSeq = rdfSeq;
        OWLClassC_.uri = uri;
    }

    public static void initMetamodelClassOWLClassD(SingularAttributeImpl<OWLClassD, OWLClassA> owlClassAAttribute, Identifier uri) {
        OWLClassD_.owlClassA = owlClassAAttribute;
        OWLClassD_.uri = uri;
        }


    public static void initMetamodelClassOWLClassQ(SingularAttributeImpl<OWLClassQ, String> stringAttribute, SingularAttributeImpl<QMappedSuperclass, String> parentStringAttribute, SingularAttributeImpl<QMappedSuperclass, String> label, SingularAttributeImpl<QMappedSuperclass, OWLClassA> owlClassAAttribute, Identifier uri) {
        OWLClassQ_.stringAttribute = stringAttribute;
        OWLClassQ_.parentString = parentStringAttribute;
        OWLClassQ_.label = label;
        OWLClassQ_.owlClassA = owlClassAAttribute;
        OWLClassQ_.uri = uri;
    }
}
