//
//  YogaKit.h
//  YogaKit
//
//  Created by Matt Lilley on 13/01/17.
//  Copyright © 2017 Matt Lilley. All rights reserved.
//

#import <UIKit/UIKit.h>

//! Project version number for YogaKit.
FOUNDATION_EXPORT double YogaKitVersionNumber;

//! Project version string for YogaKit.
FOUNDATION_EXPORT const unsigned char YogaKitVersionString[];

// In this header, you should import all the public headers of your framework using statements like #import <YogaKit/PublicHeader.h>

#include <yoga/YGMacros.h>
#include <yoga/YGEnums.h>
#include <yoga/Yoga.h>

#import <YogaKit/YGLayout.h>


@interface UIView (Yoga)

@property (nonatomic, readonly, strong) YGLayout *yoga;

@end
