//
//  Prolog.m
//  proactive
//
//  Created by Matt Lilley on 5/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import "Prolog.h"
#import <Proscript/proscript2.h>
#import "PrologSourceDownloader.h"
#import "Constants.h"

NSMutableArray* environments;
NSMutableArray* callbacks;
NSMutableDictionary* blobs; // This is a dictionary of type(string) -> NSMutableArray
NSMutableDictionary* next_free;

Options* defaultOptions;
Options* quotedOptions;

@implementation Prolog
+(void)initialize
{
    // For some unimaginable reason this does not default to the resource path when an iOS app is started
    // Since Proscript expects that test.pl will be in the current directory, use chdir() here to set it up
    chdir([[[NSBundle mainBundle] resourcePath] UTF8String]);
    init_prolog();
    environments = [[NSMutableArray alloc] init];
    callbacks = [[NSMutableArray alloc] init];
    blobs = [[NSMutableDictionary alloc] init];
    next_free = [[NSMutableDictionary alloc] init];
    defaultOptions = _create_options();
    quotedOptions = _create_options();
    _set_option(quotedOptions, [Prolog makeAtomFrom:@"quoted"], [Prolog makeAtomFrom:@"true"]);
}

+(word)localTermFromString:(NSString *)string
{
    return _string_to_local_term([string UTF8String], [string length]);
}

+(bool)module:(NSString*)module containsPredicate:(word)indicator
{
    return _exists_predicate([Prolog makeAtomFrom:module], indicator);
}
+(word)makeAtomFrom:(NSString*)string
{
    return _make_atom([string UTF8String], [string length]);
}

+(word)makeVariable
{
    return _make_variable();
}

+(word)deref:(word)term
{
    return _deref(term);
}

+(id)blobData:(word)w forType:(NSString*)type
{
    int index = _get_blob([type UTF8String], [Prolog deref:w]);
    NSMutableArray* list = blobs[type];
    id data = [list objectAtIndex:index];
    return data;
}

+(word)makeBlobOfType:(NSString*)type withData:(id)data
{
    int key;
    if (blobs[type] == nil)
    {
        blobs[type] = [[NSMutableArray alloc] initWithObjects:data, nil];
        next_free[type] = [NSNumber numberWithInt:-1];
        key = 0;
    }
    else
    {
        if ([next_free[type] integerValue] != -1)
        {
            key = [next_free[type] integerValue];
            next_free[type] = (blobs[type])[key];
        }
        [(NSMutableArray*)blobs[type] addObject:data];
        key = [blobs[type] count] - 1;
    }
    int i = _make_blob_from_index([type UTF8String], key);
    return i;
}

+(word)makeCompoundFrom:(word)functor, ...
{
    va_list argp;
    va_start(argp, functor);
    return _make_vacompound(functor, argp);
}

+(word)makeFunctor:(word)name withArity:(int)arity
{
    return _make_functor(name, arity);
}

+(Choicepoint)saveState
{
    return _push_state();
}

+(void)restoreState:(Choicepoint)state
{
    return _restore_state(state);
}

+(bool)isVariable:(word)w
{
    return _is_variable(w);
}

+(bool)isAtom:(word)w
{
    return _is_atom(w);
}

+(const char*)atomChars:(word)w
{
    return _atom_data(w);
}

+(NSString*)atomString:(word)w
{
    return [[NSString alloc] initWithBytes:_atom_data(w) length:_atom_length(w) encoding:NSASCIIStringEncoding];
}

+(bool)isCompound:(word)w
{
    return _is_compound(w);
}

+(bool)isCompound:(word)w withFunctor:(word)f
{
    return _is_compound_with_functor(w, f);
}

+(bool)isBlob:(word)w ofType:(NSString*)type
{
    return _is_blob(w, [type UTF8String]);
}

+(word)arg:(int)i ofTerm:(word)w
{
    return [Prolog deref:_term_arg([Prolog deref:w], i)];
}

void _callback(RC result)
{
    [environments removeLastObject];
    void (^cb)(int) = [callbacks lastObject];
    [callbacks removeLastObject];
    cb(result);
}

+(void)executeGoal:(word)goal inEnvironment:(id)env thenCall:(void(^)(RC))callback
{
    // FIXME: Implement
    [environments addObject:env];
    [callbacks addObject:callback];
    _execute(goal, _callback);
}

+(int)consultFile:(NSString *)filename
{
    return _consult_file([filename UTF8String]);
}

+(void)consultURL:(NSString*)url thenCall:(void(^)(int))callback
{
    NSLog(@"Downloading %@", url);
    NSURLSession *sessionWithoutADelegate = [NSURLSession sessionWithConfiguration:[NSURLSessionConfiguration defaultSessionConfiguration]];
    [[sessionWithoutADelegate dataTaskWithURL:[NSURL URLWithString:url] completionHandler:^(NSData *data, NSURLResponse *response, NSError *error)
    {
        if (error == nil)
        {
            _consult_string_of_length([data bytes], (int)[data length]);
            callback(1);
        }
        else
        {
            NSLog(@"Failed to download %@: %@", url, error);
            callback(0);
        }

    }] resume];
    
}

+(void)freeLocal:(word)w
{
    return _free_local(w);
}

+(word)makeLocal:(word)w
{
    return _make_local(w);
}

+(word)currentException
{
    return _get_exception();
}

+(NSString*)formatTerm:(word)w
{
    return [Prolog formatTerm:w withQuotes:NO];
}

+(void)releaseBlob:(word)b ofType:(NSString*)type
{
    // FIXME: Not implemented
}

+(NSString*)formatTerm:(word)w withQuotes:(bool)withQuotes
{
    char* ptr;
    int len;
    Options* options = NULL;
    if (withQuotes)
        options = quotedOptions;
    else
        options = defaultOptions;
    _format_term(options, 1200, w, &ptr, &len);
    return [[NSString alloc] initWithBytes:ptr length:len encoding:NSASCIIStringEncoding];
}

+(void)registerForeignPredicate:(word)functor inModule:(NSString*)module as:(foreign_call_t)func
{
    [Prolog registerForeignPredicate:functor inModule:module as:func withOptions:0];
}

+(void)registerForeignPredicate:(word)functor inModule:(NSString*)module as:(foreign_call_t)func withOptions:(int)flags
{
    _define_foreign_predicate([Prolog makeAtomFrom:module], functor, func, flags);
}

+(int)unify:(word)a and:(word)b
{
    return _unify(a, b);
}

+(int)setException:(word)e
{
    return _set_exception(e);
}

+(int)typeError:(word)actual whenExpecting:(word)expected
{
    return [Prolog setException:[Prolog makeCompoundFrom:[Constants errorFunctor], [Prolog makeCompoundFrom:[Constants typeErrorFunctor], expected, actual], [Prolog makeVariable]]];
}

+(word)makeNull
{
    return [Prolog makeCompoundFrom:[Constants curlyFunctor], [Constants nullAtom]];
}

@end
