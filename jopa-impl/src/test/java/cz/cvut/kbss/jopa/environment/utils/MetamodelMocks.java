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
package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.environment.OWLClassA;
import cz.cvut.kbss.jopa.environment.OWLClassB;
import cz.cvut.kbss.jopa.environment.OWLClassC;
import cz.cvut.kbss.jopa.environment.OWLClassD;
import cz.cvut.kbss.jopa.environment.OWLClassE;
import cz.cvut.kbss.jopa.environment.OWLClassF;
import cz.cvut.kbss.jopa.environment.OWLClassG;
import cz.cvut.kbss.jopa.environment.OWLClassH;
import cz.cvut.kbss.jopa.environment.OWLClassI;
import cz.cvut.kbss.jopa.environment.OWLClassJ;
import cz.cvut.kbss.jopa.environment.OWLClassK;
import cz.cvut.kbss.jopa.environment.OWLClassL;
import cz.cvut.kbss.jopa.environment.OWLClassM;
import cz.cvut.kbss.jopa.environment.OWLClassN;
import cz.cvut.kbss.jopa.environment.OWLClassO;
import cz.cvut.kbss.jopa.environment.OWLClassP;
import cz.cvut.kbss.jopa.environment.OWLClassQ;
import cz.cvut.kbss.jopa.environment.OWLClassR;
import cz.cvut.kbss.jopa.environment.OWLClassS;
import cz.cvut.kbss.jopa.environment.OWLClassT;
import cz.cvut.kbss.jopa.environment.OWLClassU;
import cz.cvut.kbss.jopa.environment.OWLClassW;
import cz.cvut.kbss.jopa.environment.OWLClassWithQueryAttr;
import cz.cvut.kbss.jopa.environment.OneOfEnum;
import cz.cvut.kbss.jopa.environment.Person;
import cz.cvut.kbss.jopa.environment.Phone;
import cz.cvut.kbss.jopa.environment.QMappedSuperclass;
import cz.cvut.kbss.jopa.environment.listener.AnotherListener;
import cz.cvut.kbss.jopa.environment.listener.ConcreteListener;
import cz.cvut.kbss.jopa.environment.listener.ParentListener;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.metamodel.AbstractAttribute;
import cz.cvut.kbss.jopa.model.metamodel.AbstractPluralAttribute;
import cz.cvut.kbss.jopa.model.metamodel.AbstractQueryAttribute;
import cz.cvut.kbss.jopa.model.metamodel.IdentifiableEntityType;
import cz.cvut.kbss.jopa.model.metamodel.Identifier;
import cz.cvut.kbss.jopa.model.metamodel.ListAttributeImpl;
import cz.cvut.kbss.jopa.model.metamodel.MappedSuperclassTypeImpl;
import cz.cvut.kbss.jopa.model.metamodel.Metamodel;
import cz.cvut.kbss.jopa.model.metamodel.PropertiesSpecification;
import cz.cvut.kbss.jopa.model.metamodel.RDFCollectionAttribute;
import cz.cvut.kbss.jopa.model.metamodel.RdfContainerAttributeImpl;
import cz.cvut.kbss.jopa.model.metamodel.SetAttributeImpl;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttribute;
import cz.cvut.kbss.jopa.model.metamodel.SingularAttributeImpl;
import cz.cvut.kbss.jopa.model.metamodel.SingularQueryAttributeImpl;
import cz.cvut.kbss.jopa.model.metamodel.TypesSpecification;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.net.URL;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Provides metamodel mock objects for the test classes.
 */
@SuppressWarnings("unused")
public class MetamodelMocks {

    @Mock
    private IdentifiableEntityType<OWLClassA> etA;
    @Mock
    private Identifier<OWLClassA, URI> idA;
    @Mock
    private SingularAttributeImpl<OWLClassA, String> aStringAtt;
    @Mock
    private TypesSpecification<OWLClassA, String> aTypes;

    @Mock
    private IdentifiableEntityType<OWLClassB> etB;
    @Mock
    private Identifier<OWLClassB, URI> idB;
    @Mock
    private SingularAttributeImpl<OWLClassB, String> bStringAtt;
    @Mock
    private PropertiesSpecification<OWLClassB, Map, String, String> bProperties;

    @Mock
    private IdentifiableEntityType<OWLClassC> etC;
    @Mock
    private Identifier<OWLClassC, URI> idC;
    @Mock
    private ListAttributeImpl<OWLClassC, OWLClassA> cReferencedList;
    @Mock
    private ListAttributeImpl<OWLClassC, OWLClassA> cSimpleList;
    @Mock
    private RDFCollectionAttribute<OWLClassC, OWLClassA> cRdfCollection;
    @Mock
    private RdfContainerAttributeImpl<OWLClassC, List<OWLClassA>, OWLClassA> cRdfSeq;

    @Mock
    private IdentifiableEntityType<OWLClassD> etD;
    @Mock
    private Identifier<OWLClassD, URI> idD;
    @Mock
    private SingularAttributeImpl<OWLClassD, OWLClassA> dOwlClassAAtt;

    @Mock
    private IdentifiableEntityType<OWLClassE> etE;
    @Mock
    private Identifier<OWLClassE, URI> idE;
    @Mock
    private SingularAttributeImpl<OWLClassE, String> eStringAtt;

    @Mock
    private IdentifiableEntityType<OWLClassG> etG;
    @Mock
    private Identifier<OWLClassG, URI> idG;
    @Mock
    private SingularAttributeImpl<OWLClassG, OWLClassH> gOwlClassHAtt;

    @Mock
    private IdentifiableEntityType<OWLClassF> etF;
    @Mock
    private Identifier<OWLClassF, URI> idF;
    @Mock
    private SingularAttributeImpl<OWLClassF, String> fStringAtt;
    @Mock
    private SetAttributeImpl<OWLClassF, OWLClassA> fSetAtt;

    @Mock
    private IdentifiableEntityType<OWLClassH> etH;
    @Mock
    private Identifier<OWLClassH, URI> idH;
    @Mock
    private SingularAttributeImpl<OWLClassH, OWLClassA> hOwlClassAAtt;
    @Mock
    private SingularAttributeImpl<OWLClassH, OWLClassG> hOwlClassGAtt;

    @Mock
    private IdentifiableEntityType<OWLClassI> etI;
    @Mock
    private Identifier<OWLClassI, URI> idI;
    @Mock
    private SingularAttributeImpl<OWLClassI, OWLClassA> iOwlClassAAtt;

    @Mock
    private IdentifiableEntityType<OWLClassJ> etJ;
    @Mock
    private Identifier<OWLClassJ, URI> idJ;
    @Mock
    private SetAttributeImpl<OWLClassJ, OWLClassA> jSetAtt;

    @Mock
    private IdentifiableEntityType<OWLClassK> etK;
    @Mock
    private Identifier<OWLClassK, URI> idK;
    @Mock
    private SingularAttributeImpl<OWLClassK, OWLClassE> kOwlClassEAtt;

    @Mock
    private IdentifiableEntityType<OWLClassL> etL;
    @Mock
    private Identifier<OWLClassL, URI> idL;
    @Mock
    private ListAttributeImpl<OWLClassL, OWLClassA> lSimpleList;
    @Mock
    private ListAttributeImpl<OWLClassL, OWLClassA> lReferencedList;
    @Mock
    private SetAttributeImpl<OWLClassL, OWLClassA> lSetAtt;
    @Mock
    private SingularAttributeImpl<OWLClassL, OWLClassA> lOwlClassAAtt;

    @Mock
    private IdentifiableEntityType<OWLClassM> etM;
    @Mock
    private Identifier<OWLClassM, String> idM;
    @Mock
    private SingularAttributeImpl<OWLClassM, Boolean> mBooleanAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, Integer> mIntegerAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, Long> mLongAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, Double> mDoubleAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, Date> mDateAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, Character> mCharacterAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, OWLClassM.Severity> mEnumAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, OWLClassM.Severity> mOrdinalEnumAtt;
    @Mock
    private SetAttributeImpl<OWLClassM, Integer> mIntegerSetAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, String> mLexicalFormAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, String> mSimpleLiteralAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, String> mExplicitDatatypeAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, ZoneOffset> mWithConverterAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, OneOfEnum> mObjectOneOfEnumAttribute;

    @Mock
    private IdentifiableEntityType<OWLClassN> etN;
    @Mock
    private Identifier<OWLClassN, String> idN;
    @Mock
    private SingularAttributeImpl<OWLClassN, String> nAnnotationAtt;
    @Mock
    private SingularAttributeImpl<OWLClassN, URI> nAnnotationUriAtt;
    @Mock
    private SingularAttributeImpl<OWLClassN, String> nStringAtt;
    @Mock
    private SetAttributeImpl<OWLClassN, String> nPluralAnnotationAtt;
    @Mock
    private PropertiesSpecification<OWLClassN, Map, String, String> nProperties;

    @Mock
    private IdentifiableEntityType<OWLClassO> etO;
    @Mock
    private Identifier<OWLClassO, URI> idO;
    @Mock
    private SingularAttributeImpl<OWLClassO, String> oStringAtt;

    @Mock
    private IdentifiableEntityType<OWLClassP> etP;
    @Mock
    private Identifier<OWLClassP, URI> idP;
    @Mock
    private SingularAttributeImpl<OWLClassP, URI> pUriAtt;
    @Mock
    private SetAttributeImpl<OWLClassP, URL> pUrlsAtt;
    @Mock
    private ListAttributeImpl<OWLClassP, URI> pSimpleList;
    @Mock
    private ListAttributeImpl<OWLClassP, URI> pReferencedList;
    @Mock
    private TypesSpecification<OWLClassP, URI> pTypes;
    @Mock
    private PropertiesSpecification<OWLClassP, Map, URI, Object> pProperties;

    @Mock
    private MappedSuperclassTypeImpl<QMappedSuperclass> qMappedSuperclass;
    @Mock
    private IdentifiableEntityType<OWLClassQ> etQ;
    @Mock
    private Identifier<OWLClassQ, URI> idQ;
    @Mock
    private SingularAttributeImpl<OWLClassQ, String> qStringAtt;
    @Mock
    private SingularAttributeImpl<QMappedSuperclass, String> qLabelAtt;
    @Mock
    private SingularAttributeImpl<QMappedSuperclass, String> qParentStringAtt;
    @Mock
    private SingularAttributeImpl<QMappedSuperclass, OWLClassA> qOwlClassAAtt;

    @Mock
    private IdentifiableEntityType<OWLClassR> etR;
    @Mock
    private SingularAttributeImpl<OWLClassR, String> rStringAtt;
    @Mock
    private SingularAttributeImpl<OWLClassR, OWLClassA> rOwlClassAAtt;
    @Mock
    private ConcreteListener concreteListenerMock;
    @Mock
    private AnotherListener anotherListenerMock;

    @Mock
    private IdentifiableEntityType<OWLClassS> etS;
    @Mock
    private Identifier<OWLClassS, URI> idS;
    @Mock
    private SingularAttributeImpl<OWLClassS, String> sNameAtt;
    @Mock
    private TypesSpecification<OWLClassS, String> sTypes;
    @Mock
    private ParentListener parentListenerMock;

    @Mock
    private IdentifiableEntityType<OWLClassT> etT;
    @Mock
    private Identifier<OWLClassT, URI> idT;
    @Mock
    private SingularAttributeImpl<OWLClassT, LocalDate> tLocalDateAtt;
    @Mock
    private SingularAttributeImpl<OWLClassT, LocalDateTime> tLocalDateTimeAtt;
    @Mock
    private SingularAttributeImpl<OWLClassT, OWLClassS> tOwlClassSAtt;

    @Mock
    private IdentifiableEntityType<OWLClassU> etU;
    @Mock
    private Identifier<OWLClassU, URI> idU;
    @Mock
    private SingularAttributeImpl<OWLClassU, MultilingualString> uSingularStringAtt;
    @Mock
    private SetAttributeImpl<OWLClassU, MultilingualString> uPluralStringAtt;
    @Mock
    private SingularAttributeImpl<OWLClassU, LocalDateTime> uModified;

    @Mock
    private IdentifiableEntityType<OWLClassW> etW;
    @Mock
    private Identifier<OWLClassW, URI> idW;
    @Mock
    private AbstractPluralAttribute<OWLClassW, Set<String>, String> wSetStringAtt;
    @Mock
    private ListAttributeImpl<OWLClassW, String> wListStringAtt;
    @Mock
    private AbstractPluralAttribute<OWLClassW, Collection<String>, String> wCollectionStringAtt;
    @Mock
    private AbstractQueryAttribute<OWLClassW, Set<String>> wSetQueryStringAtt;
    @Mock
    private AbstractQueryAttribute<OWLClassW, List<String>> wListQueryStringAtt;

    @Mock
    private IdentifiableEntityType<OWLClassWithQueryAttr> etQA;
    @Mock
    private Identifier<OWLClassWithQueryAttr, URI> idQA;
    @Mock
    private SingularQueryAttributeImpl<OWLClassWithQueryAttr, String> qaStringQueryAtt;
    @Mock
    private SingularAttributeImpl<OWLClassWithQueryAttr, String> qaStringAtt;
    @Mock
    private SingularQueryAttributeImpl<OWLClassWithQueryAttr, OWLClassA> qaEntityQueryAtt;
    @Mock
    private SingularAttributeImpl<OWLClassWithQueryAttr, OWLClassA> qaEntityAtt;

    @Mock
    private IdentifiableEntityType<Person> etPerson;
    @Mock
    private Identifier<Person, URI> idPerson;
    @Mock
    private SingularAttributeImpl<Person, String> personFirstNameAtt;
    @Mock
    private SingularAttributeImpl<Person, String> personLastNameAtt;
    @Mock
    private SingularAttributeImpl<Person, String> personUsernameAtt;
    @Mock
    private SingularAttributeImpl<Person, String> personGenderAtt;
    @Mock
    private SingularAttributeImpl<Person, Integer> personAgeAtt;
    @Mock
    private SingularAttributeImpl<Person, Phone> personPhoneAtt;
    @Mock
    private TypesSpecification<Person, String> personTypes;

    @Mock
    private IdentifiableEntityType<Phone> etPhone;
    @Mock
    private Identifier<Phone, URI> idPhone;
    @Mock
    private SingularAttributeImpl<Phone, String> phoneNumberAtt;
    @Mock
    private SingularAttributeImpl<Phone, String> phoneBrandAtt;

    public MetamodelMocks() throws Exception {
        MockitoAnnotations.openMocks(this);
        MetamodelFactory.initOWLClassAMocks(etA, aStringAtt, aTypes, idA);
        MetamodelClassInitializer.initMetamodelClassOWLClassA(aStringAtt, aTypes, idA);
        MetamodelFactory.initOWLClassBMocks(etB, bStringAtt, bProperties, idB);
        MetamodelClassInitializer.initMetamodelClassOWLClassB(bStringAtt, bProperties, idB);
        MetamodelFactory.initOWLClassCMocks(etC, cSimpleList, cReferencedList, cRdfCollection, cRdfSeq, etA, idC);
        MetamodelClassInitializer.initMetamodelClassOWLClassC(cSimpleList, cReferencedList, cRdfSeq, idC);
        MetamodelFactory.initOWLClassDMocks(etD, dOwlClassAAtt, etA, idD);
        MetamodelClassInitializer.initMetamodelClassOWLClassD(dOwlClassAAtt, idD);
        MetamodelFactory.initOWLClassEMocks(etE, eStringAtt, idE);
        MetamodelFactory.initOWLClassFMocks(etF, fSetAtt, fStringAtt, etA, idF);
        MetamodelFactory.initOWLClassGMocks(etG, gOwlClassHAtt, etH, idG);
        MetamodelFactory.initOWLClassHMocks(etH, hOwlClassAAtt, hOwlClassGAtt, etA, etG, idH);
        MetamodelFactory.initOWLClassIMocks(etI, iOwlClassAAtt, etA, idI);
        MetamodelFactory.initOWLClassJMocks(etJ, jSetAtt, etA, idJ);
        MetamodelFactory.initOWLClassKMocks(etK, kOwlClassEAtt, etE, idK);
        MetamodelFactory.initOWLClassLMocks(etL, lReferencedList, lSimpleList, lSetAtt, lOwlClassAAtt, etA, idL);
        MetamodelFactory.initOWLClassMMock(etM, mBooleanAtt, mIntegerAtt, mLongAtt, mDoubleAtt, mDateAtt, mCharacterAtt, mEnumAtt,
                                           mOrdinalEnumAtt, mIntegerSetAtt, mLexicalFormAtt, mSimpleLiteralAtt,
                                           mExplicitDatatypeAtt, mWithConverterAtt, mObjectOneOfEnumAttribute, idM);
        MetamodelFactory.initOWLClassNMock(etN, nAnnotationAtt, nAnnotationUriAtt, nStringAtt, nPluralAnnotationAtt,
                                           nProperties, idN);
        MetamodelFactory.initOWLClassOMock(etO, oStringAtt, idO);
        MetamodelFactory
                .initOWLClassPMock(etP, pTypes, pProperties, pUriAtt, pUrlsAtt, pSimpleList, pReferencedList, idP);
        MetamodelFactory
                .initOwlClassQMock(etQ, qMappedSuperclass, qStringAtt, qParentStringAtt, qLabelAtt, qOwlClassAAtt, idQ);
        MetamodelClassInitializer.initMetamodelClassOWLClassQ(qStringAtt, qParentStringAtt, qLabelAtt, qOwlClassAAtt,
                                                              idQ);
        MetamodelFactory.initOwlClassSMock(etS, sNameAtt, sTypes, idS);
        MetamodelFactory.initOwlClassSListeners(etS, parentListenerMock);
        MetamodelFactory.initOwlClassRMock(etR, rStringAtt, rOwlClassAAtt, etS);
        MetamodelFactory.initOwlClassRListeners(etR, etS, concreteListenerMock, anotherListenerMock);
        MetamodelFactory.initOwlClassTMock(etT, tLocalDateAtt, tLocalDateTimeAtt, tOwlClassSAtt, etS, idT);
        MetamodelFactory.initOwlClassUMocks(etU, uSingularStringAtt, uPluralStringAtt, uModified, idU);
        MetamodelFactory.initOwlClassWMocks(etW, wSetStringAtt, wListStringAtt, wCollectionStringAtt, wSetQueryStringAtt, wListQueryStringAtt, idW);
        MetamodelFactory.initOWLClassWithQueryAttrMocks(etQA, qaStringQueryAtt, qaStringAtt, qaEntityQueryAtt,
                                                        qaEntityAtt, etA, idQA);
        MetamodelFactory.initPhoneMocks(etPhone, phoneNumberAtt, phoneBrandAtt, idPhone);
        MetamodelFactory.initPersonMocks(etPerson, personFirstNameAtt, personLastNameAtt, personUsernameAtt, personGenderAtt, personAgeAtt, personPhoneAtt,
                                         etPhone, personTypes, idPerson);
    }

    public void setMocks(Metamodel metamodel) {
        // Order does matter for the entity() method implementation below
        final Map<Class<?>, IdentifiableEntityType<?>> etMap = new LinkedHashMap<>();
        etMap.put(OWLClassA.class, etA);
        etMap.put(OWLClassB.class, etB);
        etMap.put(OWLClassC.class, etC);
        etMap.put(OWLClassD.class, etD);
        etMap.put(OWLClassE.class, etE);
        etMap.put(OWLClassF.class, etF);
        etMap.put(OWLClassG.class, etG);
        etMap.put(OWLClassH.class, etH);
        etMap.put(OWLClassI.class, etI);
        etMap.put(OWLClassJ.class, etJ);
        etMap.put(OWLClassK.class, etK);
        etMap.put(OWLClassL.class, etL);
        etMap.put(OWLClassM.class, etM);
        etMap.put(OWLClassN.class, etN);
        etMap.put(OWLClassO.class, etO);
        etMap.put(OWLClassP.class, etP);
        etMap.put(OWLClassQ.class, etQ);
        etMap.put(OWLClassR.class, etR);
        etMap.put(OWLClassS.class, etS);
        etMap.put(OWLClassT.class, etT);
        etMap.put(OWLClassU.class, etU);
        etMap.put(OWLClassW.class, etW);
        etMap.put(OWLClassWithQueryAttr.class, etQA);
        etMap.put(Person.class, etPerson);
        etMap.put(Phone.class, etPhone);
        when(metamodel.entity(any())).thenAnswer(invocation -> {
            final Class<?> cls = (Class<?>) invocation.getArguments()[0];
            if (etMap.containsKey(cls)) {
                return etMap.get(cls);
            }
            for (Class<?> type : etMap.keySet()) {
                if (type.isAssignableFrom(cls)) {
                    return etMap.get(type);
                }
            }
            throw new IllegalArgumentException(
                    "Class " + cls.getName() + " is not a known entity in this persistence unit.");
        });
        when(metamodel.getEntities()).thenReturn(new HashSet<>(etMap.values()));
        if (metamodel instanceof MetamodelImpl) {
            etMap.keySet().forEach(cls -> when(((MetamodelImpl) metamodel).isEntityType(cls)).thenReturn(true));
        }
    }

    public OWLClassAMetamodel forOwlClassA() {
        return new OWLClassAMetamodel();
    }

    public OWLClassBMetamodel forOwlClassB() {
        return new OWLClassBMetamodel();
    }

    public OWLClassCMetamodel forOwlClassC() {
        return new OWLClassCMetamodel();
    }

    public OWLClassDMetamodel forOwlClassD() {
        return new OWLClassDMetamodel();
    }

    public OWLClassEMetamodel forOwlClassE() {
        return new OWLClassEMetamodel();
    }

    public OWLClassFMetamodel forOwlClassF() {
        return new OWLClassFMetamodel();
    }

    public OWLClassGMetamodel forOwlClassG() {
        return new OWLClassGMetamodel();
    }

    public OWLClassHMetamodel forOwlClassH() {
        return new OWLClassHMetamodel();
    }

    public OWLClassIMetamodel forOwlClassI() {
        return new OWLClassIMetamodel();
    }

    public OWLClassJMetamodel forOwlClassJ() {
        return new OWLClassJMetamodel();
    }

    public OWLClassKMetamodel forOwlClassK() {
        return new OWLClassKMetamodel();
    }

    public OWLClassLMetamodel forOwlClassL() {
        return new OWLClassLMetamodel();
    }

    public OWLClassMMetamodel forOwlClassM() {
        return new OWLClassMMetamodel();
    }

    public OWLClassNMetamodel forOwlClassN() {
        return new OWLClassNMetamodel();
    }

    public OWLClassOMetamodel forOwlClassO() {
        return new OWLClassOMetamodel();
    }

    public OWLClassPMetamodel forOwlClassP() {
        return new OWLClassPMetamodel();
    }

    public OWLClassQMetamodel forOwlClassQ() {
        return new OWLClassQMetamodel();
    }

    public OWLClassRMetamodel forOwlClassR() {
        return new OWLClassRMetamodel();
    }

    public OWLClassSMetamodel forOwlClassS() {
        return new OWLClassSMetamodel();
    }

    public OWLClassTMetamodel forOwlClassT() {
        return new OWLClassTMetamodel();
    }

    public OWLClassUMetamodel forOwlClassU() {
        return new OWLClassUMetamodel();
    }

    public OWLClassWMetamodel forOWLClassW() {
        return new OWLClassWMetamodel();
    }

    public OWLClassWithQueryAttrMetamodel forOwlClassWithQueryAttr() {
        return new OWLClassWithQueryAttrMetamodel();
    }

    public class OWLClassAMetamodel {
        public IdentifiableEntityType<OWLClassA> entityType() {
            return MetamodelMocks.this.etA;
        }

        public Identifier<OWLClassA, URI> identifier() {
            return MetamodelMocks.this.idA;
        }

        public AbstractAttribute<OWLClassA, String> stringAttribute() {
            return MetamodelMocks.this.aStringAtt;
        }

        public TypesSpecification<OWLClassA, String> typesSpec() {
            return MetamodelMocks.this.aTypes;
        }
    }

    public class OWLClassBMetamodel {
        public IdentifiableEntityType<OWLClassB> entityType() {
            return MetamodelMocks.this.etB;
        }

        public Identifier<OWLClassB, URI> identifier() {
            return MetamodelMocks.this.idB;
        }

        public AbstractAttribute<OWLClassB, String> stringAttribute() {
            return MetamodelMocks.this.bStringAtt;
        }

        public PropertiesSpecification<OWLClassB, Map, String, String> propertiesSpec() {
            return MetamodelMocks.this.bProperties;
        }
    }

    public class OWLClassCMetamodel {
        public IdentifiableEntityType<OWLClassC> entityType() {
            return MetamodelMocks.this.etC;
        }

        public Identifier<OWLClassC, URI> identifier() {
            return MetamodelMocks.this.idC;
        }

        public ListAttributeImpl<OWLClassC, OWLClassA> referencedListAtt() {
            return MetamodelMocks.this.cReferencedList;
        }

        public ListAttributeImpl<OWLClassC, OWLClassA> simpleListAtt() {
            return MetamodelMocks.this.cSimpleList;
        }

        public RDFCollectionAttribute<OWLClassC, OWLClassA> rdfCollectionAtt() {
            return MetamodelMocks.this.cRdfCollection;
        }

        public RdfContainerAttributeImpl<OWLClassC, List<OWLClassA>, OWLClassA> rdfSeqAtt() {
            return MetamodelMocks.this.cRdfSeq;
        }
    }

    public class OWLClassDMetamodel {
        public IdentifiableEntityType<OWLClassD> entityType() {
            return MetamodelMocks.this.etD;
        }

        public Identifier<OWLClassD, URI> identifier() {
            return MetamodelMocks.this.idD;
        }

        public AbstractAttribute<OWLClassD, OWLClassA> owlClassAAtt() {
            return MetamodelMocks.this.dOwlClassAAtt;
        }
    }

    public class OWLClassEMetamodel {
        public IdentifiableEntityType<OWLClassE> entityType() {
            return MetamodelMocks.this.etE;
        }

        public Identifier<OWLClassE, URI> identifier() {
            return MetamodelMocks.this.idE;
        }

        public AbstractAttribute<OWLClassE, String> stringAttribute() {
            return MetamodelMocks.this.eStringAtt;
        }
    }

    public class OWLClassFMetamodel {
        public IdentifiableEntityType<OWLClassF> entityType() {
            return MetamodelMocks.this.etF;
        }

        public Identifier<OWLClassF, URI> identifier() {
            return MetamodelMocks.this.idF;
        }

        public AbstractAttribute<OWLClassF, String> stringAttribute() {
            return MetamodelMocks.this.fStringAtt;
        }

        public AbstractPluralAttribute<OWLClassF, Set<OWLClassA>, OWLClassA> setAttribute() {
            return MetamodelMocks.this.fSetAtt;
        }
    }

    public class OWLClassGMetamodel {
        public IdentifiableEntityType<OWLClassG> entityType() {
            return MetamodelMocks.this.etG;
        }

        public Identifier<OWLClassG, URI> identifier() {
            return MetamodelMocks.this.idG;
        }

        public AbstractAttribute<OWLClassG, OWLClassH> owlClassHAtt() {
            return MetamodelMocks.this.gOwlClassHAtt;
        }
    }

    public class OWLClassHMetamodel {
        public IdentifiableEntityType<OWLClassH> entityType() {
            return MetamodelMocks.this.etH;
        }

        public Identifier<OWLClassH, URI> identifier() {
            return MetamodelMocks.this.idH;
        }

        public AbstractAttribute<OWLClassH, OWLClassG> owlClassGAtt() {
            return MetamodelMocks.this.hOwlClassGAtt;
        }

        public AbstractAttribute<OWLClassH, OWLClassA> owlClassAAtt() {
            return MetamodelMocks.this.hOwlClassAAtt;
        }
    }

    public class OWLClassIMetamodel {
        public IdentifiableEntityType<OWLClassI> entityType() {
            return MetamodelMocks.this.etI;
        }

        public Identifier<OWLClassI, URI> identifier() {
            return MetamodelMocks.this.idI;
        }

        public SingularAttributeImpl<OWLClassI, OWLClassA> owlClassAAtt() {
            return MetamodelMocks.this.iOwlClassAAtt;
        }
    }

    public class OWLClassJMetamodel {
        public IdentifiableEntityType<OWLClassJ> entityType() {
            return MetamodelMocks.this.etJ;
        }

        public Identifier<OWLClassJ, URI> identifier() {
            return MetamodelMocks.this.idJ;
        }

        public SetAttributeImpl<OWLClassJ, OWLClassA> setAttribute() {
            return MetamodelMocks.this.jSetAtt;
        }
    }

    public class OWLClassKMetamodel {
        public IdentifiableEntityType<OWLClassK> entityType() {
            return MetamodelMocks.this.etK;
        }

        public Identifier<OWLClassK, URI> identifier() {
            return MetamodelMocks.this.idK;
        }

        public AbstractAttribute<OWLClassK, OWLClassE> owlClassEAtt() {
            return MetamodelMocks.this.kOwlClassEAtt;
        }
    }

    public class OWLClassLMetamodel {
        public IdentifiableEntityType<OWLClassL> entityType() {
            return MetamodelMocks.this.etL;
        }

        public Identifier<OWLClassL, URI> identifier() {
            return MetamodelMocks.this.idL;
        }

        public ListAttributeImpl<OWLClassL, OWLClassA> referencedListAtt() {
            return MetamodelMocks.this.lReferencedList;
        }

        public ListAttributeImpl<OWLClassL, OWLClassA> simpleListAtt() {
            return MetamodelMocks.this.lSimpleList;
        }

        public AbstractPluralAttribute<OWLClassL, Set<OWLClassA>, OWLClassA> setAttribute() {
            return MetamodelMocks.this.lSetAtt;
        }

        public AbstractAttribute<OWLClassL, OWLClassA> owlClassAAtt() {
            return MetamodelMocks.this.lOwlClassAAtt;
        }
    }

    public class OWLClassMMetamodel {
        public IdentifiableEntityType<OWLClassM> entityType() {
            return MetamodelMocks.this.etM;
        }

        public Identifier<OWLClassM, String> identifier() {
            return MetamodelMocks.this.idM;
        }

        public AbstractAttribute<OWLClassM, Boolean> booleanAttribute() {
            return MetamodelMocks.this.mBooleanAtt;
        }

        public AbstractAttribute<OWLClassM, Integer> integerAttribute() {
            return MetamodelMocks.this.mIntegerAtt;
        }

        public AbstractAttribute<OWLClassM, Long> longAttribute() {
            return MetamodelMocks.this.mLongAtt;
        }

        public AbstractAttribute<OWLClassM, Double> doubleAttribute() {
            return MetamodelMocks.this.mDoubleAtt;
        }

        public AbstractAttribute<OWLClassM, Date> dateAttribute() {
            return MetamodelMocks.this.mDateAtt;
        }

        public AbstractAttribute<OWLClassM, Character> characterAttribute() {
            return MetamodelMocks.this.mCharacterAtt;
        }

        public AbstractAttribute<OWLClassM, OWLClassM.Severity> enumAttribute() {
            return MetamodelMocks.this.mEnumAtt;
        }

        public AbstractAttribute<OWLClassM, OWLClassM.Severity> ordinalEnumAttribute() {
            return MetamodelMocks.this.mOrdinalEnumAtt;
        }

        public AbstractPluralAttribute<OWLClassM, Set<Integer>, Integer> integerSetAttribute() {
            return MetamodelMocks.this.mIntegerSetAtt;
        }

        public AbstractAttribute<OWLClassM, String> lexicalFormAttribute() {
            return MetamodelMocks.this.mLexicalFormAtt;
        }

        public AbstractAttribute<OWLClassM, String> simpleLiteralAttribute() {
            return MetamodelMocks.this.mSimpleLiteralAtt;
        }

        public AbstractAttribute<OWLClassM, String> explicitDatatypeAttribute() {
            return MetamodelMocks.this.mExplicitDatatypeAtt;
        }

        public AbstractAttribute<OWLClassM, ZoneOffset> withConverterAttribute() {
            return MetamodelMocks.this.mWithConverterAtt;
        }

        public AbstractAttribute<OWLClassM, OneOfEnum> objectOneOfEnumAttribute() {
            return MetamodelMocks.this.mObjectOneOfEnumAttribute;
        }
    }

    public class OWLClassNMetamodel {
        public IdentifiableEntityType<OWLClassN> entityType() {
            return MetamodelMocks.this.etN;
        }

        public Identifier<OWLClassN, String> identifier() {
            return MetamodelMocks.this.idN;
        }

        public SingularAttributeImpl<OWLClassN, String> annotationAttribute() {
            return MetamodelMocks.this.nAnnotationAtt;
        }

        public SingularAttributeImpl<OWLClassN, URI> annotationUriAttribute() {
            return MetamodelMocks.this.nAnnotationUriAtt;
        }

        public SingularAttributeImpl<OWLClassN, String> stringAttribute() {
            return MetamodelMocks.this.nStringAtt;
        }

        public AbstractPluralAttribute<OWLClassN, Set<String>, String> pluralAnnotationAttribute() {
            return MetamodelMocks.this.nPluralAnnotationAtt;
        }

        public PropertiesSpecification<OWLClassN, Map, String, String> properties() {
            return MetamodelMocks.this.nProperties;
        }
    }

    public class OWLClassOMetamodel {
        public IdentifiableEntityType<OWLClassO> entityType() {
            return MetamodelMocks.this.etO;
        }

        public Identifier<OWLClassO, URI> identifier() {
            return MetamodelMocks.this.idO;
        }

        public SingularAttribute<OWLClassO, String> stringAttribute() {
            return MetamodelMocks.this.oStringAtt;
        }
    }

    public class OWLClassPMetamodel {
        public IdentifiableEntityType<OWLClassP> entityType() {
            return MetamodelMocks.this.etP;
        }

        public Identifier<OWLClassP, URI> identifier() {
            return MetamodelMocks.this.idP;
        }

        public AbstractAttribute<OWLClassP, URI> pUriAttribute() {
            return MetamodelMocks.this.pUriAtt;
        }

        public AbstractPluralAttribute<OWLClassP, Set<URL>, URL> pUrlsAttribute() {
            return MetamodelMocks.this.pUrlsAtt;
        }

        public ListAttributeImpl<OWLClassP, URI> pSimpleListAttribute() {
            return MetamodelMocks.this.pSimpleList;
        }

        public ListAttributeImpl<OWLClassP, URI> pReferencedListAttribute() {
            return MetamodelMocks.this.pReferencedList;
        }

        public TypesSpecification<OWLClassP, URI> types() {
            return MetamodelMocks.this.pTypes;
        }

        public PropertiesSpecification<OWLClassP, Map, URI, Object> properties() {
            return MetamodelMocks.this.pProperties;
        }
    }

    public class OWLClassQMetamodel {
        public IdentifiableEntityType<OWLClassQ> entityType() {
            return MetamodelMocks.this.etQ;
        }

        public Identifier<OWLClassQ, URI> identifier() {
            return MetamodelMocks.this.idQ;
        }

        public SingularAttribute<OWLClassQ, String> qStringAtt() {
            return MetamodelMocks.this.qStringAtt;
        }

        public SingularAttribute<QMappedSuperclass, String> qParentStringAtt() {
            return MetamodelMocks.this.qParentStringAtt;
        }

        public SingularAttribute<QMappedSuperclass, String> qLabelAtt() {
            return MetamodelMocks.this.qLabelAtt;
        }

        public SingularAttribute<QMappedSuperclass, OWLClassA> qOwlClassAAtt() {
            return MetamodelMocks.this.qOwlClassAAtt;
        }
    }

    public class OWLClassRMetamodel {
        public IdentifiableEntityType<OWLClassR> entityType() {
            return MetamodelMocks.this.etR;
        }

        public SingularAttribute<OWLClassR, String> rStringAtt() {
            return MetamodelMocks.this.rStringAtt;
        }

        public SingularAttribute<OWLClassR, OWLClassA> rOwlClassAAtt() {
            return MetamodelMocks.this.rOwlClassAAtt;
        }

        public ConcreteListener concreteListener() {
            return MetamodelMocks.this.concreteListenerMock;
        }

        public AnotherListener anotherListener() {
            return MetamodelMocks.this.anotherListenerMock;
        }
    }

    public class OWLClassSMetamodel {

        public IdentifiableEntityType<OWLClassS> entityType() {
            return MetamodelMocks.this.etS;
        }

        public Identifier<OWLClassS, URI> identifier() {
            return MetamodelMocks.this.idS;
        }

        public AbstractAttribute<OWLClassS, String> rNameAtt() {
            return MetamodelMocks.this.sNameAtt;
        }

        public TypesSpecification<OWLClassS, String> types() {
            return MetamodelMocks.this.sTypes;
        }

        public ParentListener parentListener() {
            return MetamodelMocks.this.parentListenerMock;
        }
    }

    public class OWLClassTMetamodel {
        public IdentifiableEntityType<OWLClassT> entityType() {
            return MetamodelMocks.this.etT;
        }

        public Identifier<OWLClassT, URI> identifier() {
            return MetamodelMocks.this.idT;
        }

        public AbstractAttribute<OWLClassT, LocalDate> tLocalDateAtt() {
            return MetamodelMocks.this.tLocalDateAtt;
        }

        public AbstractAttribute<OWLClassT, LocalDateTime> tLocalDateTimeAtt() {
            return MetamodelMocks.this.tLocalDateTimeAtt;
        }

        public AbstractAttribute<OWLClassT, OWLClassS> tOwlClassSAtt() {
            return MetamodelMocks.this.tOwlClassSAtt;
        }
    }

    public class OWLClassUMetamodel {
        public IdentifiableEntityType<OWLClassU> entityType() {
            return MetamodelMocks.this.etU;
        }

        public Identifier<OWLClassU, URI> identifier() {
            return MetamodelMocks.this.idU;
        }

        public AbstractAttribute<OWLClassU, MultilingualString> uSingularStringAtt() {
            return MetamodelMocks.this.uSingularStringAtt;
        }

        public AbstractPluralAttribute<OWLClassU, Set<MultilingualString>, MultilingualString> uPluralStringAtt() {
            return MetamodelMocks.this.uPluralStringAtt;
        }

        public AbstractAttribute<OWLClassU, LocalDateTime> uModified() {
            return MetamodelMocks.this.uModified;
        }
    }

    public class OWLClassWMetamodel {
        public IdentifiableEntityType<OWLClassW> entityType() {
            return MetamodelMocks.this.etW;
        }

        public Identifier<OWLClassW, URI> identifier() {
            return MetamodelMocks.this.idW;
        }

        public AbstractPluralAttribute<OWLClassW, Set<String>, String> vSetStringAtt() {
            return MetamodelMocks.this.wSetStringAtt;
        }

        public ListAttributeImpl<OWLClassW, String> vListStringAtt() {
            return MetamodelMocks.this.wListStringAtt;
        }

        public AbstractPluralAttribute<OWLClassW, Collection<String>, String> vCollectionStringAtt() {
            return MetamodelMocks.this.wCollectionStringAtt;
        }

        public AbstractQueryAttribute<OWLClassW, Set<String>> vSetQueryStringAtt() {
            return MetamodelMocks.this.wSetQueryStringAtt;
        }

        public AbstractQueryAttribute<OWLClassW, List<String>> vListQueryStringAtt() {
            return MetamodelMocks.this.wListQueryStringAtt;
        }
    }

    public class OWLClassWithQueryAttrMetamodel {
        public IdentifiableEntityType<OWLClassWithQueryAttr> entityType() {
            return MetamodelMocks.this.etQA;
        }

        public Identifier<OWLClassWithQueryAttr, URI> identifier() {
            return MetamodelMocks.this.idQA;
        }

        public AbstractAttribute<OWLClassWithQueryAttr, String> stringAttribute() {
            return MetamodelMocks.this.qaStringAtt;
        }

        public AbstractQueryAttribute<OWLClassWithQueryAttr, String> stringQueryAttribute() {
            return MetamodelMocks.this.qaStringQueryAtt;
        }

        public AbstractAttribute<OWLClassWithQueryAttr, OWLClassA> entityAttribute() {
            return MetamodelMocks.this.qaEntityAtt;
        }

        public AbstractQueryAttribute<OWLClassWithQueryAttr, OWLClassA> entityQueryAttribute() {
            return MetamodelMocks.this.qaEntityQueryAtt;
        }
    }

    public class PhoneMetamodel {
        public IdentifiableEntityType<Phone> entityType() {
            return MetamodelMocks.this.etPhone;
        }

        public Identifier<Phone, URI> identifier() {
            return MetamodelMocks.this.idPhone;
        }

        public SingularAttributeImpl<Phone, String> numberAttribute() {
            return MetamodelMocks.this.phoneNumberAtt;
        }

        public SingularAttributeImpl<Phone, String> brandAttribute() {
            return MetamodelMocks.this.phoneBrandAtt;
        }
    }

    public class PersonMetamodel {
        public IdentifiableEntityType<Person> entityType() {
            return MetamodelMocks.this.etPerson;
        }

        public Identifier<Person, URI> identifier() {
            return MetamodelMocks.this.idPerson;
        }

        public SingularAttributeImpl<Person, String> firstNameAttribute() {
            return MetamodelMocks.this.personFirstNameAtt;
        }

        public SingularAttributeImpl<Person, String> lastNameAttribute() {
            return MetamodelMocks.this.personLastNameAtt;
        }

        public SingularAttributeImpl<Person, String> usernameAttribute() {
            return MetamodelMocks.this.personUsernameAtt;
        }

        public SingularAttributeImpl<Person, String> genderAttribute() {
            return MetamodelMocks.this.personGenderAtt;
        }

        public SingularAttributeImpl<Person, Integer> ageAttribute() {
            return MetamodelMocks.this.personAgeAtt;
        }

        public SingularAttributeImpl<Person, Phone> phoneAttribute() {
            return MetamodelMocks.this.personPhoneAtt;
        }

        public TypesSpecification<Person, String> typesSpecification() {
            return MetamodelMocks.this.personTypes;
        }
    }
}
