/**
 * Copyright (C) 2022 Czech Technical University in Prague
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
package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.environment.listener.AnotherListener;
import cz.cvut.kbss.jopa.environment.listener.ConcreteListener;
import cz.cvut.kbss.jopa.environment.listener.ParentListener;
import cz.cvut.kbss.jopa.model.MetamodelImpl;
import cz.cvut.kbss.jopa.model.MultilingualString;
import cz.cvut.kbss.jopa.model.metamodel.*;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.net.URI;
import java.net.URL;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.util.*;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;

/**
 * Provides metamodel mock objects for the test classes.
 */
@SuppressWarnings("unused")
public class MetamodelMocks {

    @Mock
    private EntityTypeImpl<OWLClassA> etA;
    @Mock
    private Identifier<OWLClassA, URI> idA;
    @Mock
    private SingularAttributeImpl<OWLClassA, String> aStringAtt;
    @Mock
    private TypesSpecification<OWLClassA, String> aTypes;

    @Mock
    private EntityTypeImpl<OWLClassB> etB;
    @Mock
    private Identifier<OWLClassB, URI> idB;
    @Mock
    private SingularAttributeImpl<OWLClassB, String> bStringAtt;
    @Mock
    private PropertiesSpecification<OWLClassB, Map, String, String> bProperties;

    @Mock
    private EntityTypeImpl<OWLClassC> etC;
    @Mock
    private Identifier<OWLClassC, URI> idC;
    @Mock
    private ListAttributeImpl<OWLClassC, OWLClassA> cReferencedList;
    @Mock
    private ListAttributeImpl<OWLClassC, OWLClassA> cSimpleList;

    @Mock
    private EntityTypeImpl<OWLClassD> etD;
    @Mock
    private Identifier<OWLClassD, URI> idD;
    @Mock
    private SingularAttributeImpl<OWLClassD, OWLClassA> dOwlClassAAtt;

    @Mock
    private EntityTypeImpl<OWLClassE> etE;
    @Mock
    private Identifier<OWLClassE, URI> idE;
    @Mock
    private SingularAttributeImpl<OWLClassE, String> eStringAtt;

    @Mock
    private EntityTypeImpl<OWLClassG> etG;
    @Mock
    private Identifier<OWLClassG, URI> idG;
    @Mock
    private SingularAttributeImpl<OWLClassG, OWLClassH> gOwlClassHAtt;

    @Mock
    private EntityTypeImpl<OWLClassF> etF;
    @Mock
    private Identifier<OWLClassF, URI> idF;
    @Mock
    private SingularAttributeImpl<OWLClassF, String> fStringAtt;
    @Mock
    private AbstractPluralAttribute<OWLClassF, Set<OWLClassA>, OWLClassA> fSetAtt;

    @Mock
    private EntityTypeImpl<OWLClassH> etH;
    @Mock
    private Identifier<OWLClassH, URI> idH;
    @Mock
    private SingularAttributeImpl<OWLClassH, OWLClassA> hOwlClassAAtt;
    @Mock
    private SingularAttributeImpl<OWLClassH, OWLClassG> hOwlClassGAtt;

    @Mock
    private EntityTypeImpl<OWLClassJ> etJ;
    @Mock
    private Identifier<OWLClassJ, URI> idJ;
    @Mock
    private AbstractPluralAttribute<OWLClassJ, Set<OWLClassA>, OWLClassA> jSetAtt;

    @Mock
    private EntityTypeImpl<OWLClassK> etK;
    @Mock
    private Identifier<OWLClassK, URI> idK;
    @Mock
    private SingularAttributeImpl<OWLClassK, OWLClassE> kOwlClassEAtt;

    @Mock
    private EntityTypeImpl<OWLClassL> etL;
    @Mock
    private Identifier<OWLClassL, URI> idL;
    @Mock
    private ListAttributeImpl<OWLClassL, OWLClassA> lSimpleList;
    @Mock
    private ListAttributeImpl<OWLClassL, OWLClassA> lReferencedList;
    @Mock
    private AbstractPluralAttribute<OWLClassL, Set<OWLClassA>, OWLClassA> lSetAtt;
    @Mock
    private SingularAttributeImpl<OWLClassL, OWLClassA> lOwlClassAAtt;

    @Mock
    private EntityTypeImpl<OWLClassM> etM;
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
    private SingularAttributeImpl<OWLClassM, OWLClassM.Severity> mEnumAtt;
    @Mock
    private AbstractPluralAttribute<OWLClassM, Set<Integer>, Integer> mIntegerSetAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, String> mLexicalFormAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, String> mSimpleLiteralAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, String> mExplicitDatatypeAtt;
    @Mock
    private SingularAttributeImpl<OWLClassM, ZoneOffset> mWithConverterAtt;

    @Mock
    private EntityTypeImpl<OWLClassN> etN;
    @Mock
    private Identifier<OWLClassN, String> idN;
    @Mock
    private SingularAttributeImpl<OWLClassN, String> nAnnotationAtt;
    @Mock
    private SingularAttributeImpl<OWLClassN, URI> nAnnotationUriAtt;
    @Mock
    private SingularAttributeImpl<OWLClassN, String> nStringAtt;
    @Mock
    private AbstractPluralAttribute<OWLClassN, Set<String>, String> nPluralAnnotationAtt;
    @Mock
    private PropertiesSpecification<OWLClassN, Map, String, String> nProperties;

    @Mock
    private EntityTypeImpl<OWLClassO> etO;
    @Mock
    private Identifier<OWLClassO, URI> idO;
    @Mock
    private SingularAttributeImpl<OWLClassO, String> oStringAtt;

    @Mock
    private EntityTypeImpl<OWLClassP> etP;
    @Mock
    private Identifier<OWLClassP, URI> idP;
    @Mock
    private SingularAttributeImpl<OWLClassP, URI> pUriAtt;
    @Mock
    private AbstractPluralAttribute<OWLClassP, Set<URL>, URL> pUrlsAtt;
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
    private EntityTypeImpl<OWLClassQ> etQ;
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
    private EntityTypeImpl<OWLClassR> etR;
    @Mock
    private SingularAttributeImpl<OWLClassR, String> rStringAtt;
    @Mock
    private SingularAttributeImpl<OWLClassR, OWLClassA> rOwlClassAAtt;
    @Mock
    private ConcreteListener concreteListenerMock;
    @Mock
    private AnotherListener anotherListenerMock;

    @Mock
    private EntityTypeImpl<OWLClassS> etS;
    @Mock
    private Identifier<OWLClassS, URI> idS;
    @Mock
    private SingularAttributeImpl<OWLClassS, String> sNameAtt;
    @Mock
    private TypesSpecification<OWLClassS, String> sTypes;
    @Mock
    private ParentListener parentListenerMock;

    @Mock
    private EntityTypeImpl<OWLClassT> etT;
    @Mock
    private Identifier<OWLClassT, URI> idT;
    @Mock
    private SingularAttributeImpl<OWLClassT, LocalDate> tLocalDateAtt;
    @Mock
    private SingularAttributeImpl<OWLClassT, LocalDateTime> tLocalDateTimeAtt;
    @Mock
    private SingularAttributeImpl<OWLClassT, OWLClassS> tOwlClassSAtt;

    @Mock
    private EntityTypeImpl<OWLClassU> etU;
    @Mock
    private Identifier<OWLClassU, URI> idU;
    @Mock
    private SingularAttributeImpl<OWLClassU, MultilingualString> uSingularStringAtt;
    @Mock
    private AbstractPluralAttribute<OWLClassU, Set<MultilingualString>, MultilingualString> uPluralStringAtt;

    @Mock
    private EntityTypeImpl<OWLClassWithQueryAttr> etQA;
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

    public MetamodelMocks() throws Exception {
        MockitoAnnotations.openMocks(this);
        MetamodelFactory.initOWLClassAMocks(etA, aStringAtt, aTypes, idA);
        MetamodelClassInitializer.initMetamodelClassOWLClassA(aStringAtt, aTypes, idA);
        MetamodelFactory.initOWLClassBMocks(etB, bStringAtt, bProperties, idB);
        MetamodelClassInitializer.initMetamodelClassOWLClassB(bStringAtt, bProperties, idB);
        MetamodelFactory.initOWLClassCMocks(etC, cSimpleList, cReferencedList, idC);
        MetamodelClassInitializer.initMetamodelClassOWLClassC(cSimpleList, cReferencedList, idC);
        MetamodelFactory.initOWLClassDMocks(etD, dOwlClassAAtt, idD);
        MetamodelClassInitializer.initMetamodelClassOWLClassD(dOwlClassAAtt, idD);
        MetamodelFactory.initOWLClassEMocks(etE, eStringAtt, idE);
        MetamodelFactory.initOWLClassFMocks(etF, fSetAtt, fStringAtt, idF);
        MetamodelFactory.iniOWLClassGMocks(etG, gOwlClassHAtt, idG);
        MetamodelFactory.initOWLClassHMocks(etH, hOwlClassAAtt, hOwlClassGAtt, idH);
        MetamodelFactory.initOWLClassJMocks(etJ, jSetAtt, idJ);
        MetamodelFactory.initOWLClassKMocks(etK, kOwlClassEAtt, idK);
        MetamodelFactory.initOWLClassLMocks(etL, lReferencedList, lSimpleList, lSetAtt, lOwlClassAAtt, idL);
        MetamodelFactory.initOWLClassMMock(etM, mBooleanAtt, mIntegerAtt, mLongAtt, mDoubleAtt, mDateAtt, mEnumAtt,
                                           mIntegerSetAtt, mLexicalFormAtt, mSimpleLiteralAtt, mExplicitDatatypeAtt,
                                           mWithConverterAtt, idM);
        MetamodelFactory.initOWLClassNMock(etN, nAnnotationAtt, nAnnotationUriAtt, nStringAtt, nPluralAnnotationAtt,
                nProperties, idN);
        MetamodelFactory.initOWLClassOMock(etO, oStringAtt, idO);
        MetamodelFactory
                .initOWLClassPMock(etP, pTypes, pProperties, pUriAtt, pUrlsAtt, pSimpleList, pReferencedList, idP);
        MetamodelFactory
                .initOwlClassQMock(etQ, qMappedSuperclass, qStringAtt, qParentStringAtt, qLabelAtt, qOwlClassAAtt, idQ);
        MetamodelClassInitializer.initMetamodelClassOWLClassQ(qStringAtt, qParentStringAtt, qLabelAtt, qOwlClassAAtt, idQ);
        MetamodelFactory.initOwlClassSMock(etS, sNameAtt, sTypes, idS);
        MetamodelFactory.initOwlClassSListeners(etS, parentListenerMock);
        MetamodelFactory.initOwlClassRMock(etR, rStringAtt, rOwlClassAAtt, etS);
        MetamodelFactory.initOwlClassRListeners(etR, etS, concreteListenerMock, anotherListenerMock);
        MetamodelFactory.initOwlClassTMock(etT, tLocalDateAtt, tLocalDateTimeAtt, tOwlClassSAtt, idT);
        MetamodelFactory.initOwlClassUMocks(etU, uSingularStringAtt, uPluralStringAtt, idU);
        MetamodelFactory.initOWLClassWithQueryAttrMocks(etQA, qaStringQueryAtt, qaStringAtt, qaEntityQueryAtt, qaEntityAtt, idQA);
    }

    public void setMocks(Metamodel metamodel) {
        // Order does matter for the entity() method implementation below
        final Map<Class<?>, EntityTypeImpl<?>> etMap = new LinkedHashMap<>();
        etMap.put(OWLClassA.class, etA);
        etMap.put(OWLClassB.class, etB);
        etMap.put(OWLClassC.class, etC);
        etMap.put(OWLClassD.class, etD);
        etMap.put(OWLClassE.class, etE);
        etMap.put(OWLClassF.class, etF);
        etMap.put(OWLClassG.class, etG);
        etMap.put(OWLClassH.class, etH);
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
        etMap.put(OWLClassWithQueryAttr.class, etQA);
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

    public OWLClassWithQueryAttrMetamodel forOwlClassWithQueryAttr() {
        return new OWLClassWithQueryAttrMetamodel();
    }

    public class OWLClassAMetamodel {
        public EntityTypeImpl<OWLClassA> entityType() {
            return MetamodelMocks.this.etA;
        }

        public Identifier<OWLClassA, URI> identifier() {
            return MetamodelMocks.this.idA;
        }

        public AbstractAttribute<OWLClassA, String> stringAttribute() {
            return MetamodelMocks.this.aStringAtt;
        }

        public TypesSpecification<OWLClassA, ?> typesSpec() {
            return MetamodelMocks.this.aTypes;
        }
    }

    public class OWLClassBMetamodel {
        public EntityTypeImpl<OWLClassB> entityType() {
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
        public EntityTypeImpl<OWLClassC> entityType() {
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
    }

    public class OWLClassDMetamodel {
        public EntityTypeImpl<OWLClassD> entityType() {
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
        public EntityTypeImpl<OWLClassE> entityType() {
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
        public EntityTypeImpl<OWLClassF> entityType() {
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
        public EntityTypeImpl<OWLClassG> entityType() {
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
        public EntityTypeImpl<OWLClassH> entityType() {
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

    public class OWLClassJMetamodel {
        public EntityTypeImpl<OWLClassJ> entityType() {
            return MetamodelMocks.this.etJ;
        }

        public Identifier<OWLClassJ, URI> identifier() {
            return MetamodelMocks.this.idJ;
        }

        public AbstractPluralAttribute<OWLClassJ, Set<OWLClassA>, OWLClassA> setAttribute() {
            return MetamodelMocks.this.jSetAtt;
        }
    }

    public class OWLClassKMetamodel {
        public EntityTypeImpl<OWLClassK> entityType() {
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
        public EntityTypeImpl<OWLClassL> entityType() {
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
        public EntityTypeImpl<OWLClassM> entityType() {
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

        public AbstractAttribute<OWLClassM, OWLClassM.Severity> enumAttribute() {
            return MetamodelMocks.this.mEnumAtt;
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
    }

    public class OWLClassNMetamodel {
        public EntityTypeImpl<OWLClassN> entityType() {
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
        public EntityTypeImpl<OWLClassO> entityType() {
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
        public EntityTypeImpl<OWLClassP> entityType() {
            return MetamodelMocks.this.etP;
        }

        public Identifier<OWLClassP, URI> identifier() {
            return MetamodelMocks.this.idP;
        }

        public SingularAttribute<OWLClassP, URI> pUriAttribute() {
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
        public EntityTypeImpl<OWLClassQ> entityType() {
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
        public EntityTypeImpl<OWLClassR> entityType() {
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

        public EntityTypeImpl<OWLClassS> entityType() {
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
        public EntityTypeImpl<OWLClassT> entityType() {
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
        public EntityTypeImpl<OWLClassU> entityType() {
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
    }

    public class OWLClassWithQueryAttrMetamodel {
        public EntityTypeImpl<OWLClassWithQueryAttr> entityType() {
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
}
