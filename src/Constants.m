//
//  Constants.m
//  proactive
//
//  Created by Matt Lilley on 6/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "Constants.h"

word emptyListatom = 0;
word curlyAtom = 0;
word nullAtom = 0;
word prologStateKeyAtom = 0;
word atomAtom = 0;
word listAtom = 0;
word attributeAtom = 0;

word equalsFunctor = 0;
word renderFunctor = 0;
word getInitialStateFunctor = 0;
word crossModuleFunctor = 0;
word expandChildrenFunctor = 0;
word curlyFunctor = 0;
word commaFunctor = 0;
word colonFunctor = 0;
word errorFunctor = 0;
word typeErrorFunctor = 0;
word listFunctor = 0;
word documentFunctor = 0;
word createElementFromVdomFunctor = 0;
word vdiffFunctor = 0;
word vpatchFunctor = 0;

@implementation Constants
+(void)initialize
{
    emptyListatom = [Prolog makeAtomFrom:@"[]"];
    curlyAtom = [Prolog makeAtomFrom:@"{}"];
    nullAtom = [Prolog makeAtomFrom:@"null"];
    prologStateKeyAtom = [Prolog makeAtomFrom:@"prolog_state_key"];
    atomAtom = [Prolog makeAtomFrom:@"atom"];
    attributeAtom = [Prolog makeAtomFrom:@"attribute"];
    listAtom = [Prolog makeAtomFrom:@"list"];

    
    renderFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@"render"] withArity:3];
    getInitialStateFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@"getInitialState"] withArity:2];
    crossModuleFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@":"] withArity:2];
    expandChildrenFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@"expand_children"] withArity:2];
    curlyFunctor = [Prolog makeFunctor:curlyAtom withArity:1];
    commaFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@","] withArity:2];
    colonFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@":"] withArity:2];
    errorFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@"error"] withArity:2];
    typeErrorFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@"type_error"] withArity:2];
    documentFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@"document"] withArity:1];
    listFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@"."] withArity:2];
    createElementFromVdomFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@"create_element_from_vdom"] withArity:3];
    equalsFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@"="] withArity:2];
    vdiffFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@"vdiff"] withArity:3];
    vpatchFunctor = [Prolog makeFunctor:[Prolog makeAtomFrom:@"vpatch"] withArity:4];

}

+(word)vdiffFunctor
{
    return vdiffFunctor;
}
+(word)vpatchFunctor
{
    return vpatchFunctor;
}

+(word) atomAtom
{
    return atomAtom;
}

+(word) attributeAtom
{
    return attributeAtom;
}

+(word) listAtom
{
    return listAtom;
}

+(word)equalsFunctor
{
    return equalsFunctor;
}

+(word)documentFunctor
{
    return documentFunctor;
}

+(word)listFunctor
{
    return listFunctor;
}

+(word)createElementFromVdomFunctor
{
    return createElementFromVdomFunctor;
}

+(word)renderFunctor
{
    return renderFunctor;
}

+(word)getInitialStateFunctor
{
    return getInitialStateFunctor;
}

+(word)crossModuleFunctor
{
    return crossModuleFunctor;
}

+(word) expandChildrenFunctor
{
    return expandChildrenFunctor;
}

+(word)emptyListAtom
{
    return emptyListatom;
}

+(word)nullAtom
{
    return nullAtom;
}

+(word)curlyAtom
{
    return curlyAtom;
}

+(word)curlyFunctor
{
    return curlyFunctor;
}

+(word)commaFunctor
{
    return commaFunctor;
}

+(word)colonFunctor
{
    return colonFunctor;
}

+(word)errorFunctor
{
    return errorFunctor;
}

+(word)typeErrorFunctor
{
    return typeErrorFunctor;
}

+(word)prologStateKeyAtom
{
    return prologStateKeyAtom;
}

+(word)makeNull
{
    return [Prolog makeCompoundFrom:[Constants curlyFunctor], [Constants nullAtom]];
}

+(bool)isNull:(word) w
{
    return ([Prolog isCompound:w withFunctor:[Constants curlyFunctor]] && [Prolog arg:0 ofTerm:w] == [Constants nullAtom]);
}


@end
