// To parse this JSON:
//
//   NSError *error;
//   QTHistoricalMinute *historicalMinute = [QTHistoricalMinute fromJSON:json encoding:NSUTF8Encoding error:&error];

#import <Foundation/Foundation.h>

@class QTHistoricalMinute;
@class QTDefinitions;
@class QTCandle;
@class QTAnyOf;
@class QTData;
@class QTDataProperties;
@class QTCandles;
@class QTItems;
@class QTDataClass;
@class QTHistoricalMinuteClass;
@class QTHistoricalMinuteProperties;

NS_ASSUME_NONNULL_BEGIN

#pragma mark - Object interfaces

@interface QTHistoricalMinute : NSObject
@property (nonatomic, copy)   NSString *ref;
@property (nonatomic, copy)   NSString *schema;
@property (nonatomic, strong) QTDefinitions *definitions;

+ (_Nullable instancetype)fromJSON:(NSString *)json encoding:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
+ (_Nullable instancetype)fromData:(NSData *)data error:(NSError *_Nullable *)error;
- (NSString *_Nullable)toJSON:(NSStringEncoding)encoding error:(NSError *_Nullable *)error;
- (NSData *_Nullable)toData:(NSError *_Nullable *)error;
@end

@interface QTDefinitions : NSObject
@property (nonatomic, strong) QTCandle *candle;
@property (nonatomic, strong) QTData *data;
@property (nonatomic, strong) QTHistoricalMinuteClass *historicalMinute;
@end

@interface QTCandle : NSObject
@property (nonatomic, copy) NSArray<QTAnyOf *> *anyOf;
@property (nonatomic, copy) NSString *title;
@end

@interface QTAnyOf : NSObject
@property (nonatomic, copy) NSString *type;
@end

@interface QTData : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTDataProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataProperties : NSObject
@property (nonatomic, strong) QTCandles *candles;
@end

@interface QTCandles : NSObject
@property (nonatomic, strong) QTItems *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTItems : NSObject
@property (nonatomic, strong) QTDataClass *items;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTDataClass : NSObject
@property (nonatomic, copy) NSString *ref;
@end

@interface QTHistoricalMinuteClass : NSObject
@property (nonatomic, assign) BOOL isAdditionalProperties;
@property (nonatomic, strong) QTHistoricalMinuteProperties *properties;
@property (nonatomic, copy)   NSArray<NSString *> *required;
@property (nonatomic, copy)   NSString *title;
@property (nonatomic, copy)   NSString *type;
@end

@interface QTHistoricalMinuteProperties : NSObject
@property (nonatomic, strong) QTDataClass *data;
@property (nonatomic, strong) QTAnyOf *status;
@end

NS_ASSUME_NONNULL_END
