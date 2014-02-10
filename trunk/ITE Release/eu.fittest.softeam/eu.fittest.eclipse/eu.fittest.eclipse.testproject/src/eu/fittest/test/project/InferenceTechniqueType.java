/**
 */
package eu.fittest.test.project;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.eclipse.emf.common.util.AbstractEnumerator;

/**
 * <!-- begin-user-doc -->
 * A representation of the literals of the enumeration '<em><b>Inference Technique Type</b></em>',
 * and utility methods for working with them.
 * <!-- end-user-doc -->
 * @see eu.fittest.test.project.ProjectPackage#getInferenceTechniqueType()
 * @model extendedMetaData="name='InferenceTechniqueType'"
 * @generated
 */
public final class InferenceTechniqueType extends AbstractEnumerator {
	/**
	 * The '<em><b>SEQUENCEBASED</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>SEQUENCEBASED</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #SEQUENCEBASED_LITERAL
	 * @model literal="SEQUENCE-BASED"
	 * @generated
	 * @ordered
	 */
	public static final int SEQUENCEBASED = 0;

	/**
	 * The '<em><b>STATEBASED</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>STATEBASED</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #STATEBASED_LITERAL
	 * @model literal="STATE-BASED"
	 * @generated
	 * @ordered
	 */
	public static final int STATEBASED = 1;

	/**
	 * The '<em><b>AUTOABS</b></em>' literal value.
	 * <!-- begin-user-doc -->
	 * <p>
	 * If the meaning of '<em><b>AUTOABS</b></em>' literal object isn't clear,
	 * there really should be more of a description here...
	 * </p>
	 * <!-- end-user-doc -->
	 * @see #AUTOABS_LITERAL
	 * @model literal="AUTO-ABS"
	 * @generated
	 * @ordered
	 */
	public static final int AUTOABS = 2;

	/**
	 * The '<em><b>SEQUENCEBASED</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #SEQUENCEBASED
	 * @generated
	 * @ordered
	 */
	public static final InferenceTechniqueType SEQUENCEBASED_LITERAL = new InferenceTechniqueType(SEQUENCEBASED, "SEQUENCEBASED", "SEQUENCE-BASED");

	/**
	 * The '<em><b>STATEBASED</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #STATEBASED
	 * @generated
	 * @ordered
	 */
	public static final InferenceTechniqueType STATEBASED_LITERAL = new InferenceTechniqueType(STATEBASED, "STATEBASED", "STATE-BASED");

	/**
	 * The '<em><b>AUTOABS</b></em>' literal object.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @see #AUTOABS
	 * @generated
	 * @ordered
	 */
	public static final InferenceTechniqueType AUTOABS_LITERAL = new InferenceTechniqueType(AUTOABS, "AUTOABS", "AUTO-ABS");

	/**
	 * An array of all the '<em><b>Inference Technique Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private static final InferenceTechniqueType[] VALUES_ARRAY =
		new InferenceTechniqueType[] {
			SEQUENCEBASED_LITERAL,
			STATEBASED_LITERAL,
			AUTOABS_LITERAL,
		};

	/**
	 * A public read-only list of all the '<em><b>Inference Technique Type</b></em>' enumerators.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static final List VALUES = Collections.unmodifiableList(Arrays.asList(VALUES_ARRAY));

	/**
	 * Returns the '<em><b>Inference Technique Type</b></em>' literal with the specified literal value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static InferenceTechniqueType get(String literal) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			InferenceTechniqueType result = VALUES_ARRAY[i];
			if (result.toString().equals(literal)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Inference Technique Type</b></em>' literal with the specified name.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static InferenceTechniqueType getByName(String name) {
		for (int i = 0; i < VALUES_ARRAY.length; ++i) {
			InferenceTechniqueType result = VALUES_ARRAY[i];
			if (result.getName().equals(name)) {
				return result;
			}
		}
		return null;
	}

	/**
	 * Returns the '<em><b>Inference Technique Type</b></em>' literal with the specified integer value.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	public static InferenceTechniqueType get(int value) {
		switch (value) {
			case SEQUENCEBASED: return SEQUENCEBASED_LITERAL;
			case STATEBASED: return STATEBASED_LITERAL;
			case AUTOABS: return AUTOABS_LITERAL;
		}
		return null;
	}

	/**
	 * Only this class can construct instances.
	 * <!-- begin-user-doc -->
	 * <!-- end-user-doc -->
	 * @generated
	 */
	private InferenceTechniqueType(int value, String name, String literal) {
		super(value, name, literal);
	}

} //InferenceTechniqueType
