package cz.cvut.kbss.jopa.environment.utils;

import cz.cvut.kbss.jopa.environment.*;
import cz.cvut.kbss.jopa.model.metamodel.*;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.Date;
import java.util.Set;

import static org.mockito.Mockito.when;

/**
 * Provides metamodel mock objects for the test classes.
 */
public class MetamodelMocks {

    @Mock
    private EntityType<OWLClassA> etA;
    @Mock
    private Identifier idA;
    @Mock
    private SingularAttribute<OWLClassA, String> aStringAtt;
    @Mock
    private TypesSpecification<OWLClassA, ?> aTypes;

    @Mock
    private EntityType<OWLClassB> etB;
    @Mock
    private Identifier idB;
    @Mock
    private SingularAttribute<OWLClassB, String> bStringAtt;
    @Mock
    private PropertiesSpecification<OWLClassB, ?> bProperties;

    @Mock
    private EntityType<OWLClassC> etC;
    @Mock
    private Identifier idC;
    @Mock
    private ListAttribute<OWLClassC, OWLClassA> cReferencedList;
    @Mock
    private ListAttribute<OWLClassC, OWLClassA> cSimpleList;

    @Mock
    private EntityType<OWLClassD> etD;
    @Mock
    private Identifier idD;
    @Mock
    private SingularAttribute<OWLClassD, OWLClassA> dOwlClassAAtt;

    @Mock
    private EntityType<OWLClassE> etE;
    @Mock
    private Identifier idE;
    @Mock
    private SingularAttribute<OWLClassE, String> eStringAtt;

    @Mock
    private EntityType<OWLClassJ> etJ;
    @Mock
    private Identifier idJ;
    @Mock
    private PluralAttribute<OWLClassJ, Set, OWLClassA> jSetAtt;

    @Mock
    private EntityType<OWLClassK> etK;
    @Mock
    private Identifier idK;
    @Mock
    private SingularAttribute<OWLClassK, OWLClassE> kOwlClassEAtt;

    @Mock
    private EntityType<OWLClassL> etL;
    @Mock
    private Identifier idL;
    @Mock
    private ListAttribute<OWLClassL, OWLClassA> lSimpleList;
    @Mock
    private ListAttribute<OWLClassL, OWLClassA> lReferencedList;
    @Mock
    private PluralAttribute<OWLClassL, Set, OWLClassA> lSetAtt;
    @Mock
    private SingularAttribute<OWLClassL, OWLClassA> lOwlClassAAtt;

    @Mock
    private EntityType<OWLClassM> etM;
    @Mock
    private Identifier idM;
    @Mock
    private SingularAttribute<OWLClassM, Boolean> mBooleanAtt;
    @Mock
    private SingularAttribute<OWLClassM, Integer> mIntegerAtt;
    @Mock
    private SingularAttribute<OWLClassM, Long> mLongAtt;
    @Mock
    private SingularAttribute<OWLClassM, Double> mDoubleAtt;
    @Mock
    private SingularAttribute<OWLClassM, Date> mDateAtt;
    @Mock
    private SingularAttribute<OWLClassM, OWLClassM.Severity> mEnumAtt;

    public MetamodelMocks() throws Exception {
        MockitoAnnotations.initMocks(this);
        MetamodelFactory.initOWLClassAMocks(etA, aStringAtt, aTypes, idA);
        MetamodelFactory.initOWLClassBMocks(etB, bStringAtt, bProperties, idB);
        MetamodelFactory.initOWLClassCMocks(etC, cSimpleList, cReferencedList, idC);
        MetamodelFactory.initOWLClassDMocks(etD, dOwlClassAAtt, idD);
        MetamodelFactory.initOWLClassEMocks(etE, eStringAtt, idE);
        MetamodelFactory.initOWLClassJMocks(etJ, jSetAtt, idJ);
        MetamodelFactory.initOWLClassKMocks(etK, kOwlClassEAtt, idK);
        MetamodelFactory.initOWLClassLMocks(etL, lReferencedList, lSimpleList, lSetAtt, lOwlClassAAtt, idL);
        MetamodelFactory
                .initOWLClassMMock(etM, mBooleanAtt, mIntegerAtt, mLongAtt, mDoubleAtt, mDateAtt, mEnumAtt, idM);
    }

    public void setMocks(Metamodel metamodel) {
        when(metamodel.entity(OWLClassA.class)).thenReturn(etA);
        when(metamodel.entity(OWLClassB.class)).thenReturn(etB);
        when(metamodel.entity(OWLClassC.class)).thenReturn(etC);
        when(metamodel.entity(OWLClassD.class)).thenReturn(etD);
        when(metamodel.entity(OWLClassE.class)).thenReturn(etE);
        when(metamodel.entity(OWLClassJ.class)).thenReturn(etJ);
        when(metamodel.entity(OWLClassK.class)).thenReturn(etK);
        when(metamodel.entity(OWLClassL.class)).thenReturn(etL);
        when(metamodel.entity(OWLClassM.class)).thenReturn(etM);
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

    public class OWLClassAMetamodel {
        public EntityType<OWLClassA> entityType() {
            return MetamodelMocks.this.etA;
        }

        public Identifier identifier() {
            return MetamodelMocks.this.idA;
        }

        public SingularAttribute<OWLClassA, String> stringAttribute() {
            return MetamodelMocks.this.aStringAtt;
        }

        public TypesSpecification<OWLClassA, ?> typesSpec() {
            return MetamodelMocks.this.aTypes;
        }
    }

    public class OWLClassBMetamodel {
        public EntityType<OWLClassB> entityType() {
            return MetamodelMocks.this.etB;
        }

        public Identifier identifier() {
            return MetamodelMocks.this.idB;
        }

        public SingularAttribute<OWLClassB, String> stringAttribute() {
            return MetamodelMocks.this.bStringAtt;
        }

        public PropertiesSpecification<OWLClassB, ?> propertiesSpec() {
            return MetamodelMocks.this.bProperties;
        }
    }

    public class OWLClassCMetamodel {
        public EntityType<OWLClassC> entityType() {
            return MetamodelMocks.this.etC;
        }

        public Identifier identifier() {
            return MetamodelMocks.this.idC;
        }

        public ListAttribute<OWLClassC, OWLClassA> referencedListAtt() {
            return MetamodelMocks.this.cReferencedList;
        }

        public ListAttribute<OWLClassC, OWLClassA> simpleListAtt() {
            return MetamodelMocks.this.cSimpleList;
        }
    }

    public class OWLClassDMetamodel {
        public EntityType<OWLClassD> entityType() {
            return MetamodelMocks.this.etD;
        }

        public Identifier identifier() {
            return MetamodelMocks.this.idD;
        }

        public SingularAttribute<OWLClassD, OWLClassA> owlClassAAtt() {
            return MetamodelMocks.this.dOwlClassAAtt;
        }
    }

    public class OWLClassEMetamodel {
        public EntityType<OWLClassE> entityType() {
            return MetamodelMocks.this.etE;
        }

        public Identifier identifier() {
            return MetamodelMocks.this.idE;
        }

        public SingularAttribute<OWLClassE, String> stringAttribute() {
            return MetamodelMocks.this.eStringAtt;
        }
    }

    public class OWLClassJMetamodel {
        public EntityType<OWLClassJ> entityType() {
            return MetamodelMocks.this.etJ;
        }

        public Identifier identifier() {
            return MetamodelMocks.this.idJ;
        }

        public PluralAttribute<OWLClassJ, Set, OWLClassA> setAttribute() {
            return MetamodelMocks.this.jSetAtt;
        }
    }

    public class OWLClassKMetamodel {
        public EntityType<OWLClassK> entityType() {
            return MetamodelMocks.this.etK;
        }

        public Identifier identifier() {
            return MetamodelMocks.this.idK;
        }

        public SingularAttribute<OWLClassK, OWLClassE> owlClassEAtt() {
            return MetamodelMocks.this.kOwlClassEAtt;
        }
    }

    public class OWLClassLMetamodel {
        public EntityType<OWLClassL> entityType() {
            return MetamodelMocks.this.etL;
        }

        public Identifier identifier() {
            return MetamodelMocks.this.idL;
        }

        public ListAttribute<OWLClassL, OWLClassA> referencedListAtt() {
            return MetamodelMocks.this.lReferencedList;
        }

        public ListAttribute<OWLClassL, OWLClassA> simpleListAtt() {
            return MetamodelMocks.this.lSimpleList;
        }

        public PluralAttribute<OWLClassL, Set, OWLClassA> setAttribute() {
            return MetamodelMocks.this.lSetAtt;
        }

        public SingularAttribute<OWLClassL, OWLClassA> owlClassAAtt() {
            return MetamodelMocks.this.lOwlClassAAtt;
        }
    }

    public class OWLClassMMetamodel {
        public EntityType<OWLClassM> entityType() {
            return MetamodelMocks.this.etM;
        }

        public Identifier identifier() {
            return MetamodelMocks.this.idM;
        }

        public SingularAttribute<OWLClassM, Boolean> booleanAttribute() {
            return MetamodelMocks.this.mBooleanAtt;
        }

        public SingularAttribute<OWLClassM, Integer> integerAttribute() {
            return MetamodelMocks.this.mIntegerAtt;
        }

        public SingularAttribute<OWLClassM, Long> longAttribute() {
            return MetamodelMocks.this.mLongAtt;
        }

        public SingularAttribute<OWLClassM, Double> doubleAttribute() {
            return MetamodelMocks.this.mDoubleAtt;
        }

        public SingularAttribute<OWLClassM, Date> dateAttribute() {
            return MetamodelMocks.this.mDateAtt;
        }

        public SingularAttribute<OWLClassM, OWLClassM.Severity> enumAttribute() {
            return MetamodelMocks.this.mEnumAtt;
        }
    }
}
